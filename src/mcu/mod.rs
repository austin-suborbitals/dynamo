extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use syntax::tokenstream;
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
    let mut builder = builder::Builder::new(info, parser, verbose);
    syntax::ext::base::MacEager::items(SmallVector::many(builder.build()))
}
