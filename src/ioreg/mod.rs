extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;
extern crate aster;

use syntax::tokenstream;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::quote::rt::DUMMY_SP;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};

use std;
use std::collections::BTreeMap;

pub mod parser;
pub mod common;
pub mod builder;

use parser::StaticValue;


//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, parser, false)
}

pub fn expand_ioreg_debug(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, parser, true)
}


//
// ioreg parsing
//

/// Parses offsets into the given ioreg offset and all associated functions.
///
/// The block being parsed are those of the form `1..5 => { some_func => [...]; other_func => [...]; };`.
///
/// **NOTE:** this will move into the parser proper soon.
fn parse_offset(parser: &mut parser::Parser, seg: &mut common::IoRegSegmentInfo) -> Result<common::IoRegOffsetInfo, &'static str> {
    let start_span = parser.parser.span;

    // parse the index width and begin offset
    let offset_index = parser.parse_index();
    let last_bit = offset_index.offset + offset_index.width;
    if last_bit > seg.reg_width.as_u8() {
        parser.set_err(format!("index ({}) + width ({}) = {} cannot fit in register of size {}",
            offset_index.offset, offset_index.width, last_bit, seg.reg_width.as_u8()).as_str());
    }

    // we expect a fat arrow and a curly brace to kick things off
    parser.expect_fat_arrow();
    parser.expect_open_curly();

    // loop (parsing function defs) until we hit our closing bracket
    let mut func_defs =  BTreeMap::<String, common::IoRegFuncDef>::new();
    while ! parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        // get a name and a colon to start the definition
        let name = parser.parse_ident_string();
        if ! func_defs.contains_key(&name) {    // check the function has not been registered TODO: check globally
            let func = parser.parse_func_def(name.clone(), &seg.reg_width);
            if func.ty == common::FunctionType::Setter && seg.access_perms == common::RegisterPermissions::ReadOnly {
                return Err("cannot create setter function on read only segment");
            }

            func_defs.insert(name, func);
        } else {
            return Err("duplicate function definition");
        }
    }

    Ok(common::IoRegOffsetInfo{
        index: offset_index,
        functions: func_defs,
        span: start_span,
    })

}



/// Determine if a given parser::StaticValue can fit in the needed register
fn fits_into(val: &::parser::StaticValue, width: &common::RegisterWidth) -> bool {
    match val {
        &::parser::StaticValue::Error(_, _) => { false }
        &::parser::StaticValue::Ident(_, _, _) | &::parser::StaticValue::Path(_, _) => { false }
        &::parser::StaticValue::Int(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (i8::max_value() as i32) }
                &common::RegisterWidth::R16 => { i <= (i16::max_value() as i32) }
                &common::RegisterWidth::R32 => { i <= (i32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &::parser::StaticValue::Uint(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (u8::max_value() as u32) }
                &common::RegisterWidth::R16 => { i <= (u16::max_value() as u32) }
                &common::RegisterWidth::R32 => { i <= (u32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &::parser::StaticValue::Float(f, _, _, _) => {
            match width {
                &common::RegisterWidth::R8 | &common::RegisterWidth::R16 => { false } // TODO: what to do about f8 and f16
                &common::RegisterWidth::R32 => { f <= std::f32::MAX }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &::parser::StaticValue::Str(_, _, _) => { true } // TODO: this is because we "don't care" but perhaps we should?
    }
}

/// Validate parsed constants using `fits_into()`.
fn validate_constant(
    width: &common::RegisterWidth, val: &::parser::StaticValue, _: &mut BTreeMap<String, StaticValue>
)
    -> Result<(), String>
{
    if ! fits_into(&val, width) {
        Err(format!("given literal does not fit into register of size {:?}", width))
    } else {
        Ok(())
    }
}


/// Parses an entire segment block i.e. `0x1234 => some_segment r16 ro { ... };`.
///
/// While "segments" are typically registers in and of themselves, they are more often than not accessed as a logical group.
/// This grouping allows multiple related registers to be accessed as if they were one large (contiguous) register.
///
/// **NOTE:** this will move into the parser proper soon.
fn parse_segment(parser: &mut parser::Parser) -> common::IoRegSegmentInfo {
    let start_span = parser.parser.span;

    // get an address
    let addr = parser.parse_uint::<u32>() as u32; // parse the address
    parser.begin_segment = parser.parser.span;    // save this segment's span
    parser.expect_fat_arrow();                  // skip the =>

    // gather metadata
    let name = parser.parse_ident_string();     // get the name
    let width = parser.parse_reg_width();       // get the width
    let perms = parser.parse_reg_access();      // get read/write type

    // expect the opening of offset blocks
    parser.expect_open_curly();                 // expect the opening brace

    // see if we have a constants block, and if so, parse it
    let mut val_defs: BTreeMap<String, StaticValue> = BTreeMap::new();
    if is_ident!(parser.curr_token()) {
        let tok = extract_ident_name!(parser);
        match tok.as_str() {
            "constants" => {
                parser.parse_constants_block(&name, &mut val_defs, validate_constant, &width);
            }
            _ => { parser.set_err("unexpected block keyword"); }
        }
    }

    let mut result = common::IoRegSegmentInfo{
        name: name,
        address: addr,
        reg_width: width,
        access_perms: perms,
        const_vals: val_defs,
        offsets: vec!(),
        span: start_span,
    };

    // loop until we hit our closing brace
    while ! parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        match parse_offset(parser, &mut result) {
            Ok(o) => { result.push_offset(o); }
            Err(e) => {
                parser.set_err(e);  // TODO: and early escape?
            }
        }
    }

    // expect the end of offset blocks
    parser.expect_semi();                       // ends with a semicolon
    result
}

/// Entry to parsing the entire ioreg!() expansion.
///
/// **NOTE:** this will move into the parser proper soon.
fn parse_ioreg(parser: &mut parser::Parser) -> common::IoRegInfo {
    let start_span = parser.parser.span;

    // parse the name of the ioreg
    parser.expect_ident_value("name");              // expect 'name' literal
    parser.expect_fat_arrow();                      // skip the =>
    let name_str = parser.parse_ident_string();       // get the name for the ioreg
    parser.expect_semi();

    let init = {
        if parser.parser.this_token_to_string() == "init".to_string() {
            let sp = parser.parser.span.clone();
            parser.parser.bump();
            parser.expect_fat_arrow();
            let init_item = parser.parser.parse_impl_item();
            parser.expect_semi();
            common::InitInfo{
                defined: true,
                item: match init_item {
                    Ok(i) => { Some(i) }
                    Err(mut e) => { e.emit(); None }
                },
                span: sp
            }
        } else {
            common::InitInfo{defined:false, item:None, span: DUMMY_SP}
        }
    };

    // check if we have a constants or doc_srcs definition block
    let mut const_vals: BTreeMap<String, StaticValue> = BTreeMap::new();
    let mut doc_srcs: Vec<String> = vec!();
    while is_ident!(parser.curr_token()) {
        let tok = extract_ident_name!(parser);
        match tok.as_str() {
            "constants" => {
                parser.parse_constants_block(
                    &"".to_string(), &mut const_vals,
                    validate_constant, &common::RegisterWidth::R32
                );
            }
            "doc_srcs" => { parser.parse_doc_sources(&"".to_string(), &mut doc_srcs); }
            _ => { parser.set_err(format!("unexpected block keyword '{}'", tok).as_str()); break; }
        }
    }

    // create the info struct we will parse into
    let mut result = common::IoRegInfo{
        name: name_str,
        doc_srcs: doc_srcs,
        segments: BTreeMap::new(),
        const_vals: const_vals,
        init: init,
        span: start_span,
    };

    // the rest of the macro should be segments
    while ! parser.eat(&token::Token::Eof) {
        let seg = parse_segment(parser);
        if ! result.segments.contains_key(seg.name.as_str()) {
            result.segments.insert(seg.name.clone(), seg);
        } else {
            parser.set_segment_err(format!("duplicate section named '{}'", seg.name).as_str());
        }
    }

    result
}


//
// ioreg generation
//

fn generate_ioreg(_: &ExtCtxt, info: common::IoRegInfo, parser: parser::Parser, verbose: bool) -> Box<MacResult + 'static> {
    let builder = builder::Builder::new(info, parser, verbose);

    // now, generate code from the struct and get back Vec<ast::Item> to add to the token tree
    let items = builder.build();
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
