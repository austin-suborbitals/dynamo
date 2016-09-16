extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;
extern crate aster;

use syntax::tokenstream;
use syntax::codemap::Span;
use syntax::util::small_vector::SmallVector;
use syntax::ext::base::{ExtCtxt, MacResult};

pub mod parser;
pub mod common;
pub mod builder;



//
// ioreg expansion
//

pub fn expand_ioreg(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parser.parse_ioreg();
    generate_ioreg(cx, ioreg_info, parser, false)
}

pub fn expand_ioreg_debug(cx: &mut ExtCtxt, _: Span, args: &[tokenstream::TokenTree]) -> Box<MacResult + 'static> {
    let mut parser = parser::Parser::from(cx, args);
    let ioreg_info = parser.parse_ioreg();
    generate_ioreg(cx, ioreg_info, parser, true)
}


fn generate_ioreg(_: &ExtCtxt, info: common::IoRegInfo, parser: parser::Parser, verbose: bool) -> Box<MacResult + 'static> {
    let builder = builder::Builder::new(info, parser, verbose);

    // now, generate code from the struct and get back Vec<ast::Item> to add to the token tree
    let items = builder.build();
    syntax::ext::base::MacEager::items(SmallVector::many(items.clone()))
}
