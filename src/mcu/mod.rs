/*

mcu!(
    name => SomeMcuName;
    doc_srcs => [
        "http://some.url.com/path/to/probably.pdf",
        "http://some.url.com/path/to/another.pdf",
    ];

    constants => {
        i2c_loc => 0x8000;
    };

    // TODO: externs sections

    interrupts => {
        0       => main;
        1..5    => some_common_handler;
        6       => another_handler;
        7..127  => None;
    };

    stack => {
        base    => 0x1000;
        limit   => STACK_LIMIT; // externally defined const. could easily be an internal constant
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
