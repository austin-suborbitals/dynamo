/*

mcu!(
    name => SomeMcuName;
    doc_srcs => [
        "http://some.url.com/path/to/probably.pdf",
        "http://some.url.com/path/to/another.pdf",
    ];

    constants => {
        i2c_loc = 0x8000;
    };

    // TODO: externs sections

    interrupts => [255] @ .interrupts {     // NOTE: argument to @ _must_ be a link section
        0       => main;
        1..5    => some_common_handler;
        6       => another_handler;
        7..127  => None;
    };

    // NOTE: the `base` field requires a link section to place the base pointer -- generally right before the IVT
    //       the stack **will not** be located at the link section, simply a pointer to it.
    //       it is entirely reasonable to have something like:
    //          `base => .stack_begin @ .stack_ptr`
    //       this would place the stack base at `.stack_begin` and create a constant STACK_BEGIN
    stack => {
        base    => 0x1000 @ .stack_ptr;
        limit   => STACK_LIMIT;             // externally defined const. could also be an internal constant or literal
    };

    data => {
        src         => data_flash;
        dest_begin  => data_section;
        dest_end    => data_section_end;
    };

    peripherals => {
        wdog    => wdog::Watchdog @ 0x5000;     // type parsed as a path and pointer set to value after @
        uart    => uart::UART @ UART_1;         // can also be a const/extern
        i2c     => i2c::I2C @ i2c_loc;          // can also be an internal constant
    };

    actions => [
        // NOTE: functions with empty () args after the ident get an implicit `&mut self`
        //
        // NOTE: actions cannot have return values
        //
        // NOTE: init is a special cased action.
        //       if it is defined we will not generate one -- if it is not defined, the generated MCU::init() would
        //          1. unlock the watchdog (assumes self.wdog.unlock() exists)
        //          2. disable the watchdog (assumes self.wdog.disable() exists)
        //          3. memcpy the .data section to RAM using generated copy_data_section() method
        //
        // NOTE: this function should be the first thing called in the reset_handler.
        //       from there, additional peripherals can be setup and/or app-code-jumping can be implemented
        init() => {
            // use a local peripheral to unlock the wdog and disable it
            self.wdog.unlock();
            self.wdog.disable()

            // use the generated copy_data_section() function to copy the .data section to RAM
            self.copy_data_section();

            // use a self-defined action in another action!!!
            self.enable_hardfloat();
        };


        // NOTE: this function also has implicit self
        enable_hardfloat() => {
            self.my_periph.enable();
        }

        // sets the given pin to digital mode.
        // just an example -- so probably not realistic
        digital_pin(pin: u8) =>() {
            match pin / PINS_PER_PORT {
                1 => { self.port_a.digital(pin % PINS_PER_PORT); }
                2 => { self.port_a.digital(pin % PINS_PER_PORT); }
                _ => { panic!("could not find pin"); }
            }
        }
    ];
);

*/



extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use syntax::ast;
use syntax::tokenstream;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};

pub mod common;
pub mod builder;
pub mod parser;

//
// mcu expansion
//

pub fn expand_mcu(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let mcu_info = parser.parse();
    generate_mcu(cx, mcu_info, parser, false)
}

pub fn expand_mcu_debug(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let mcu_info = parser.parse();
    generate_mcu(cx, mcu_info, parser, true)
}


//
// mcu generation
//

fn generate_mcu(_: &ExtCtxt, info: common::McuInfo, parser: parser::Parser, verbose: bool)
    -> Box<MacResult + 'static>
{
    let builder = builder::Builder::new(info, parser, verbose);

    //let items = builder.build();
    syntax::ext::base::MacEager::items(SmallVector::many(builder.build()))
}
