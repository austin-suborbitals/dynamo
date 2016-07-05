/*
    Example of an IO Register definition:

    ioreg!(
        // comments must be on their own line -- nothing parsed after '//'
        // statements end with a ';'
        // address _must_ be in hex

        // name this register WDOG
        name => WDOG;

        // this address is a 16 bit (r16) register located at 0x40052000.
        // it is named "status" and the name will be used in field/function names on WDOG.
        // it is readable, so we will have a WDOG::read_status() function to read the entire register.
        0x40052000 => status r16 rw {
            // this is a one-bit register location.
            // we can still define functions and getters as normal, but the operation in code
            // will be similar to `*(address) |= val` (not literally, but operating byte-wise as shown).
            0 => {
                // here we define the values for enabled and disabled as used by this register location.
                // these variables (static) will be added as:
                //      WDOG::status_enabled_value
                //      WDOG::status_disabled_value
                // NOTE: definitions of values do not use '=>' but rather a simple equality.
                disabled = 0;
                enabled = 1;

                // we also define functions to enable and disable the watchdog by writing
                // the above values to the register.
                // NOTE: the capital in the function name will make it a function,
                //       but will not be capitalized in code.
                //       this example would give us:  WDOG.enable() and WDOG.disable()
                Enable => [enabled];
                Disable => [disabled];
            }

            1 => {
                LPO_clock = 0;
                alternate_clock = 1;

                Use_lpo_clock => [LPO_clock];
                Use_alternate_clock => [alternate_clock];
            }

            2 => {
                reset_only = 0;
                irq_before_reset = 1;

                Default_reset_mode => [alternate_clock];
                Interrupt_before_reset => [irq_before_reset];
            }

            3 => {
                // NOTE: we have already defined enabled/disabled, so use those!
                Enable_windowing => [enabled];
                Disable_windowing => [disabled];
            }

            4 => {
                // NOTE: we have already defined enabled/disabled, so use those!
                Allow_updates => [enabled];
                Disable_updates => [disabled];
            }

            5 => {
                Enable_in_debug => [enabled];
                Disable_in_debug => [disabled];
            }

            6 => {
                Enable_in_stop => [enabled];
                Disable_in_stop => [disabled];
            }

            7 => {
                Enable_in_wait => [enabled];
                Disable_in_wait => [disabled];
            }

            // NOTE: perhaps 8 and 9 is reserved.... omit them :)

            10 => {
                Enable_test_mode => [enabled];
            }

            11 => {
                quick_test = 0;
                byte_test = 1;

                Enable_quick_test => [quick_test];
                Enable_byte_test => [byte_test];
            }

            12..13 => {
                test_byte_zero  = 0;
                test_byte_one   = 1;
                test_byte_two   = 2;
                test_byte_three = 3;

                Use_test_byte_zero => [test_byte_zero];
                Use_test_byte_one => [test_byte_one];
                Use_test_byte_two => [test_byte_two];
                Use_test_byte_three => [test_byte_three];
            }

            14 => {
                Disable_test_mode => [enabled];
                Ensable_test_mode => [disabled];
            }
        }


        // this address is a 16bit (r16) register located at 0x4005200E.
        // it is named "unlock" and will be used in field/function names on WDOG.
        // it is write only, and will get no ::read_unlock() function.
        0x4005200E => unlock r16 wo {
            0..15 => {
                // define the Unlock function that writes the given sequence of
                // values serially to this given register+offset
                // NOTE: the capital in the function name will make it a function,
                //       but will not be capitalized in code.
                //       this example would give us:  WDOG.unlock()
                // NOTE: function names must be unique across the register, not just addresses
                Unlock => [0xC520, 0xD928];
            }
        }
    )
*/

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;
extern crate aster;

use syntax::ast;
use syntax::ptr::P;
use syntax::parse::token;
use syntax::print::pprust;
use syntax::codemap::Span;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};

use std::collections::HashMap;

pub mod parser;
pub mod common;

//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, _: Span, args: &[ast::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, false)
}

pub fn expand_ioreg_debug(cx: &mut ExtCtxt, _: Span, args: &[ast::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info, true)
}


//
// ioreg parsing
//

macro_rules! read_val_def_value {
    ($parser:ident, $width:ident) => {{
        match $width {
            &common::RegisterWidth::R8 =>    { $parser.checked_parse_uint::<u8>("u8") }
            &common::RegisterWidth::R16 =>   { $parser.checked_parse_uint::<u16>("u16") }
            &common::RegisterWidth::R32 =>   { $parser.checked_parse_uint::<u32>("u32") }
            &common::RegisterWidth::R64 =>   { $parser.checked_parse_uint::<u64>("u64") }
            &common::RegisterWidth::Unknown => {
                Err("cannot read value for register of unspecified size".to_string())
            }
        }
    }}
}

fn parse_func_def(parser: &mut parser::Parser, name: String, width: &common::RegisterWidth) -> common::IoRegFuncDef {
    parser.expect_fat_arrow();                  // functions must be followed with fat arrow
    parser.expect_open_bracket();               // expect a series of values

    // until the end of the values
    let mut vals: Vec<common::FunctionValueType> = vec!();
    while ! parser.eat(&token::CloseDelim(token::DelimToken::Bracket)) {

        // inspect the token we are currently considering
        match parser.raw_parser().token {
            // if we have a literal, we expect it to be an integral, so parse that
            token::Token::Literal(token::Lit::Integer(_), _) => {  // TODO: consider float
                match read_val_def_value!(parser, width) {
                    Ok(i) => { vals.push(common::FunctionValueType::Static(i as usize)); }
                    Err(e) => {
                        parser.set_err(e.as_str());
                        break;  // TODO: better return
                    }
                };
            }

            // otherwise, we expect an ident to reference a defined variable
            token::Ident(_) => {
                vals.push(common::FunctionValueType::Reference(parser.parse_lit_string()));
            }

            // consider everything else an error
            _ => {
                parser.set_err("unexepected token type"); // TODO: better error message
            }
        }


        // TODO: this currently allows [,,,,]
        parser.eat(&token::Token::Comma);       // skip a comma if there is one
    }
    parser.expect_semi();                       // expect a close to the definition

    common::IoRegFuncDef{
        name: name,
        values: vals,
        ty: common::FunctionType::Setter,
    }
}

fn parse_val_def(parser: &mut parser::Parser, name: String, width: &common::RegisterWidth) -> common::IoRegValDef {
    parser.expect_equal();
    let val = match read_val_def_value!(parser, width) {
        Ok(i) => { i as usize }
        Err(e) => {
            parser.set_err(format!("expected a value but got error: {}", e).as_str());
            0
        }
    };
    parser.expect_semi();

    common::IoRegValDef{
        name: name,
        value: val,
    }
}

fn parse_offset(parser: &mut parser::Parser, prefix: &String, width: &common::RegisterWidth)
    -> Result<common::IoRegOffsetInfo, &'static str> {

    // parse the index width and begin offset
    let index_width = parser.parse_index();

    // we expect a fat arrow and a curly brace to kick things off
    parser.expect_fat_arrow();
    parser.expect_open_curly();

    // loop until we hit our closing bracket
    let mut val_defs: Vec<common::IoRegValDef> = vec!();
    let mut func_defs: HashMap<String, common::IoRegFuncDef> = HashMap::new();
    while ! parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        // get a name and a colon to start the definition
        let mut name = parser.parse_lit_string();
        let iter = name.chars().next().expect("expected a non-empty string literal");
        match iter.is_uppercase() {
            true => {
                name = name.to_lowercase();
                if ! func_defs.contains_key(&name) {
                    func_defs.insert(name.clone(), parse_func_def(parser, name, width));
                } else {
                    return Err("duplicate function definition");
                }
            }
            false => {
                val_defs.push(parse_val_def(parser, format!("{}_{}", prefix, name), width));
            }
        }
    }

    Ok(common::IoRegOffsetInfo{
        width: index_width.1,
        access_perms: common::RegisterPermissions::ReadOnly,
        const_vals: val_defs,
        functions: func_defs,
    })
}


// parse a segment of the register block.
// these are typically _actual_ registers.... but for code sanity
// we group them together in logical structs.
fn parse_segment(parser: &mut parser::Parser) -> common::IoRegSegmentInfo {
    // get an address
    let addr = parser.parse_uint::<u32>("u32") as u32; // parse the address
    parser.begin_segment = parser.curr_span;    // save this segment's span
    parser.expect_fat_arrow();                  // skip the =>

    // gather metadata
    let name = parser.parse_lit_string();       // get the name
    let width = parser.parse_reg_width();       // get the width
    let perms = parser.parse_reg_access();      // get read/write type

    // expect the opening of offset blocks
    parser.expect_open_curly();                 // expect the opening brace

    // loop until we hit our closing brace
    let mut offsets: Vec<common::IoRegOffsetInfo> = vec!();
    while ! parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        match parse_offset(parser, &name, &width) {
            Ok(o) => { offsets.push(o); }
            Err(e) => {
                parser.set_err(e);  // TODO: and early escape?
            }
        }
    }

    // expect the end of offset blocks
    parser.expect_semi();                       // ends with a semicolon
    common::IoRegSegmentInfo{
        name: name,
        address: addr,
        reg_width: width,
        access_perms: perms,
        offsets: offsets,
    }
}


// entry to the ioreg macro parsing
fn parse_ioreg(parser: &mut parser::Parser) -> common::IoRegInfo {
    // parse the name of the ioreg
    parser.expect_ident_value("name");              // expect 'name' literal
    parser.expect_fat_arrow();                      // skip the =>
    let name_str = parser.parse_lit_string();       // get the name for the ioreg
    parser.expect_semi();

    // create the info struct we will parse into
    let mut result = common::IoRegInfo{
        name: name_str,
        regions:HashMap::new()
    };

    // the rest of the macro should be segments
    while ! parser.eat(&token::Token::Eof) {
        let seg = parse_segment(parser);
        if ! result.regions.contains_key(seg.name.as_str()) {
            result.regions.insert(seg.name.clone(), seg);
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
    let builder = aster::AstBuilder::new();
    let info_struct = builder.item().struct_(info.name.clone())
        .field("address").ty().usize()
        .build();

    if verbose {
        println!("\n\n=====   Generated: {}   =====\n", info.name);
        println!("{}", pprust::item_to_string(&info_struct));
        println!("\n\nFrom:\n    {:?}\n\n", info);
    }

    let items: Vec<P<ast::Item>> = vec![info_struct];
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
