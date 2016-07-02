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

use std::fmt::Debug;

use syntax::ast;
use syntax::ptr::P;
use syntax::print::pprust;
use syntax::codemap::Span;
use syntax::parse::token;
use syntax::parse::parser::Parser;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};


#[derive(Debug)]
struct IoRegInfo {
    name: String,
    address: u32,   // TODO: usize?
}


//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, sp: Span, args: &[ast::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = cx.new_parser_from_tts(args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info)
}


//
// ioreg parsing
//

fn expect_ident(expect: &str, parser: &mut Parser) {
    let curr_span = parser.span;
    match parser.parse_ident() {
        Ok(i) => {
            if i.name.as_str() != expect {
                parser.span_err(curr_span, format!("expected '{}' but found '{}'", expect, i.name.as_str()).as_str());
            }
        }
        Err(e) => {
            parser.span_err(curr_span, e.message())
        }
    }
}

fn expect_semi(parser: &mut Parser) {
    match parser.expect(&token::Token::Semi) {
        Ok(i) => {
            // do nothing
        }
        Err(e) => {
            parser.span_err(parser.span, e.message());
        }
    }
}

fn get_ident(parser: &mut Parser) -> ast::Ident {
    let curr_span = parser.span;
    let builder = aster::AstBuilder::new();
    match parser.parse_ident() {
        Ok(i) => { i }
        Err(e) => {
            parser.span_err(curr_span, e.message());
            ast::Ident::with_empty_ctxt(builder.name("error"))
        }
    }
}


type LitSpan = (ast::Lit, Span);

macro_rules! get_literal {
    ($lit:ident, $span:ident, $parser:ident) => {
        let $span = $parser.span;
        let $lit = match $parser.parse_lit() {
            Ok(i) => { i }
            Err(e) => {
                // place the error
                $parser.span_err($span, e.message());
                // make a "default" to be ignored
                aster::AstBuilder::new().lit().usize(0).unwrap()
            }
        }
    }
}

fn get_int_literal(parser: &mut Parser) -> ast::Lit {
    get_literal!(lit, curr_span, parser);

    match lit.node {
        ast::LitKind::Int(_,_) => {
            // nothing
        }

        _ => {
            parser.span_err(curr_span, "expected an unsigned integral literal");
        }
    }

    lit
}

fn u32_from_lit(lit: ast::LitKind) -> u32 {
    if let ast::LitKind::Int(value, _) = lit {
        return value as u32;
    } else {
        panic!("not a u32 lit");
        0
    }
}

fn parse_ioreg(parser: &mut Parser) -> IoRegInfo {
    //
    // parse the basic info
    //

    // parse the name of the ioreg
    expect_ident("name", parser);       // expect 'name' literal
    parser.eat(&token::FatArrow);       // skip the =>
    let name_str = get_ident(parser);   // get the name for the ioreg
    expect_semi(parser);

    // get an address
    let addr = get_int_literal(parser);

    IoRegInfo{
        name: name_str.name.as_str().to_string(),
        address: u32_from_lit(addr.node)
    }
}


//
// ioreg generation
//

fn generate_ioreg(cx: &ExtCtxt, info: IoRegInfo) -> Box<MacResult + 'static> {
    let builder = aster::AstBuilder::new();
    let info_struct = builder.item().struct_(info.name.clone())
        .field("address").ty().usize()
        .build();

    println!("\n\n=====   Generated: {}   =====\n", info.name);
    println!("{}", pprust::item_to_string(&info_struct));
    println!("\n\nFrom: {:?}\n\n", info);

    let items: Vec<P<ast::Item>> = vec![info_struct];
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
