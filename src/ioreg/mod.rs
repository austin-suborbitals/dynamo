/*
    Example of an IO Register definition:

    ioreg!(
        NOTE: in the Parser's context, this is the beginning of the "ioreg block"

        // name this register WDOG
        name => WDOG;

        // here we define the const values needed by this register (particularly in functions).
        // these variables (static) will be added as: WDOG::status_{var_name}.
        // the prefix is because these values belong to the `status` segment.
        // NOTE: definitions of values do not use '=>' but rather a simple equality.
        constants => {
            // standard defines
            enabled = 1;
            disabled = 0;
        };

        // this address is a 16 bit (r16) register located at 0x40052000.
        // it is named "status" and the name will be used in field/function names on WDOG.
        // it is readable, so we will have a WDOG::read_status() function to read the entire register.
        0x40052000 => status r16 rw {
            // we can also create constants on the segment-level.
            // NOTE: these constants will be prefixed with the segment name ("status_{FOO}" in this case).
            //       using these values in the segment definition requires adding the prefix.
            // NOTE: these constants must also fit inside the register they are for. in this case an 8/16bit value.
            constants => {
                // wdog clock source
                LPO_clock = 0;
                alternate_clock = 1;

                // watchdog reset style
                reset_only = 0;
                irq_before_reset = 1;

                // test mode
                quick_test = 0;
                byte_test = 1;
                test_byte_zero  = 0;
                test_byte_one   = 1;
                test_byte_two   = 2;
                test_byte_three = 3;
            };

            // this is a one-bit register location.
            // we can still define functions and getters as normal, but the operation in code
            // will be similar to `*(address) |= val` (not literally, but operating byte-wise as shown).
            0 => {
                // we also define functions to enable and disable the watchdog by writing
                // the values defined above to the register.
                //       this example would give us:  WDOG.enable() and WDOG.disable()
                enable => [enabled];
                disable => [disabled];
            }

            1 => {
                use_lpo_clock => [status_LPO_clock];
                use_alternate_clock => [status_alternate_clock];
            }

            2 => {
                default_reset_mode => [status_reset_only];
                interrupt_before_reset => [status_irq_before_reset];
            }

            // NOTE: perhaps 3 through 10 are reserved.... omit them :)

            11 => {
                enable_quick_test => [status_quick_test];
                enable_byte_test => [status_byte_test];
            }

            12..13 => {
                use_test_byte_zero => [status_test_byte_zero];
                use_test_byte_one => [status_test_byte_one];
                use_test_byte_two => [status_test_byte_two];
                use_test_byte_three => [status_test_byte_three];
            }

            14 => {
                disable_test_mode => [enabled];
                ensable_test_mode => [disabled];
            }
        }


        // this address is a 16bit (r16) register located at 0x4005200E.
        // it is named "unlock" and will be used in field/function names on WDOG.
        // it is write only, and will get no ::read_unlock() function.
        0x4005200E => unlock r16 wo {
            NOTE: in the Parser's context, this is the beginning of a "segment block"

            0..15 => {
                NOTE: in the Parser's context, this is the beginning of an "offset block"

                // define the `unlock()` function that writes the given sequence of
                // values serially to this given register+offset
                // NOTE: function names must be unique across the register, not just addresses
                unlock => [0xC520, 0xD928];
            }
        }
    )
*/

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;
extern crate aster;


use syntax::tokenstream;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};

use std::collections::HashMap;

pub mod parser;
pub mod common;
pub mod builder;


macro_rules! is_ident {
    ($val:expr) => {
        match $val {
            &token::Token::Ident(_) => { true }
            _ => { false }
        }
    }
}

macro_rules! extract_ident_name {
    ($parser:ident) => {
        match $parser.curr_token() {
            &token::Token::Ident(i) => { i.name.as_str().to_string().clone() } // TODO: these coercions are gross
            _ => { $parser.set_err("detected an ident, but did not parse as an ident"); "".to_string() } // TODO: better default
        }
    }
}


//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, false)
}

pub fn expand_ioreg_debug(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, true)
}


//
// ioreg parsing
//

fn parse_offset(parser: &mut parser::Parser, seg: &mut common::IoRegSegmentInfo) -> Result<common::IoRegOffsetInfo, &'static str> {
    let start_span = parser.curr_span;

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
    let mut func_defs =  HashMap::<String, common::IoRegFuncDef>::new();
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


// parse a segment of the register block.
// these are typically _actual_ registers.... but for code sanity
// we group them together in logical structs.
fn parse_segment(parser: &mut parser::Parser) -> common::IoRegSegmentInfo {
    let start_span = parser.curr_span;

    // get an address
    let addr = parser.parse_uint::<u32>() as u32; // parse the address
    parser.begin_segment = parser.curr_span;    // save this segment's span
    parser.expect_fat_arrow();                  // skip the =>

    // gather metadata
    let name = parser.parse_ident_string();       // get the name
    let width = parser.parse_reg_width();       // get the width
    let perms = parser.parse_reg_access();      // get read/write type

    // expect the opening of offset blocks
    parser.expect_open_curly();                 // expect the opening brace

    // see if we have a constants block, and if so, parse it
    let mut val_defs: HashMap<String, common::StaticValue> = HashMap::new();
    if is_ident!(parser.curr_token()) {
        let tok = extract_ident_name!(parser);
        match tok.as_str() {
            "constants" => { parser.parse_constants_block(&name, &mut val_defs, &width); }
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

// entry to the ioreg macro parsing
fn parse_ioreg(parser: &mut parser::Parser) -> common::IoRegInfo {
    let start_span = parser.curr_span;

    // parse the name of the ioreg
    parser.expect_ident_value("name");              // expect 'name' literal
    parser.expect_fat_arrow();                      // skip the =>
    let name_str = parser.parse_ident_string();       // get the name for the ioreg
    parser.expect_semi();

    // check if we have a constants or doc_srcs definition block
    let mut const_vals: HashMap<String, common::StaticValue> = HashMap::new();
    let mut doc_srcs: Vec<String> = vec!();
    while is_ident!(parser.curr_token()) {
        let tok = extract_ident_name!(parser);
        match tok.as_str() {
            "constants" => { parser.parse_constants_block(&"".to_string(), &mut const_vals, &common::RegisterWidth::R32); }
            "doc_srcs" => { parser.parse_doc_sources(&"".to_string(), &mut doc_srcs); }
            _ => { parser.set_err("unexpected block keyword"); break; }
        }
    }

    // create the info struct we will parse into
    let mut result = common::IoRegInfo{
        name: name_str,
        doc_srcs: doc_srcs,
        segments: HashMap::new(),
        const_vals: const_vals,
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

fn generate_ioreg(_: &ExtCtxt, info: common::IoRegInfo, verbose: bool) -> Box<MacResult + 'static> {
    let builder = builder::Builder::new(info, verbose);

    // now, generate code from the struct and get back Vec<ast::Item> to add to the token tree
    let items = builder.build();
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
