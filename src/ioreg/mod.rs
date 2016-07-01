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
use syntax::codemap::Span;
use syntax::parse::token;
use syntax::parse::parser::Parser;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};


struct IoRegInfo {
    name: String,
    address: u32,   // TODO: usize?
}

//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, sp: Span, args: &[ast::TokenTree]) -> Box<MacResult + 'static> {
    println!("expanding ioreg");
    let mut parser = cx.new_parser_from_tts(args);
    let ioreg_info = parse_ioreg(&mut parser);
    generate_ioreg(cx, ioreg_info)
}


//
// ioreg parsing
//

fn parse_ioreg(parser: &mut Parser) -> IoRegInfo {
    let expect_name_keyword = parser.parse_ident().unwrap(); // TODO: error checking
    if expect_name_keyword.name.as_str() != "name" {
        panic!("expected 'name' identifier, but got {}", expect_name_keyword.name);
    }

    // skip the =>
    parser.eat(&token::FatArrow);

    let name_str = parser.parse_ident().unwrap(); // TODO: error checking

    println!("found {} as a name", name_str.name);

    IoRegInfo{
        name: name_str.name.as_str().to_string(),
        address: 0,
    }
}


//
// ioreg generation
//

fn generate_ioreg(cx: &ExtCtxt, info: IoRegInfo) -> Box<MacResult + 'static> {
    println!("generating ioreg");

    let builder = aster::AstBuilder::new();
    let info_struct = builder.item().struct_(info.name)
        .field("address").ty().usize()
        .build();

    println!("{}", syntax::print::pprust::item_to_string(&info_struct));

    let items: Vec<P<ast::Item>> = vec![info_struct];
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
