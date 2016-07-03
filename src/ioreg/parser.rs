use syntax::ast;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::parse::parser as rsparse;

extern crate aster;

use super::common as common;

macro_rules! parse_uint {
    ($dest:ty, $dest_enum:pat, $dest_max:expr, $dest_str:expr, $lit:ident) => {{
        if let ast::LitKind::Int(value, ty) = $lit {
            match ty {
                // we have a $dest
                $dest_enum => {
                    Ok(value as $dest)
                }
                // some unspecified int
                ast::LitIntType::Unsuffixed => {
                    if value <= ($dest_max as u64) { // TODO: better assertion
                        Ok(value as $dest)
                    } else {
                        Err(format!("found value is larger than {}", $dest_str))
                    }
                }
                // everything else
                _ => {
                    Err(format!("expected an {}, but parsed {:?}", $dest_str, ty))
                }
            }
        } else {
            Err(format!("expected a {}", $dest_str))
        }
    }};

    // TODO: better way to do this
    (u8,  $lit:ident) => {
        parse_uint!(u8, ast::LitIntType::Unsigned(ast::UintTy::U8),   u8::max_value(), "u8", $lit)
    };
    (u16, $lit:ident) => {
        parse_uint!(u16, ast::LitIntType::Unsigned(ast::UintTy::U16), u16::max_value(), "u16", $lit)
    };
    (u32, $lit:ident) => {
        parse_uint!(u32, ast::LitIntType::Unsigned(ast::UintTy::U32), u32::max_value(), "u32", $lit)
    };
    (u64, $lit:ident) => {
        parse_uint!(u64, ast::LitIntType::Unsigned(ast::UintTy::U64), u64::max_value(), "u64", $lit)
    };
}



pub struct Parser<'a> {
    parser:     rsparse::Parser<'a>,
    builder:    aster::AstBuilder,  // used for default value construction

    // state
    curr_span:  Span,
    depth:      usize,
}


impl<'a> Parser<'a> {
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[ast::TokenTree]) -> Parser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        Parser {
            parser: p,
            builder: aster::AstBuilder::new(),
            curr_span: s,
            depth: 0,
        }
    }

    pub fn set_err(&mut self, err: &str) {
        self.parser.span_err(self.curr_span, err);
    }

    pub fn current_depth(&self) -> usize { self.depth }


    //
    // state saving
    //

    fn save_span(&mut self) {
        self.curr_span = self.parser.span;
    }


    //
    // assert and consume
    //

    pub fn expect_ident_value(&mut self, expect: &str) {
        let got = self.parse_lit_string();
        if expect != got {
            self.set_err(format!("expected '{}' but found '{}'", expect, got).as_str());
        }
    }

    pub fn expect_semi(&mut self) {
        self.save_span();
        match self.parser.expect(&token::Token::Semi) {
            Ok(_) => { }
            Err(e) => { self.set_err(e.message()); }
        }
    }

    pub fn expect_fat_arrow(&mut self) {
        self.save_span();
        match self.parser.eat(&token::FatArrow) {
            true => {}
            false => { self.set_err("expected a fat arrow (=>)"); }
        }
    }

    pub fn expect_open_curly(&mut self) {
        self.save_span();
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Brace)) {
            true => { }
            false => { self.set_err("expected an opening curly brace"); }
        }
    }

    pub fn expect_close_curly(&mut self) {
        self.save_span();
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            true => { }
            false => { self.set_err("expected a closing curly brace"); }
        }
    }


    //
    // simple read
    //

    fn get_ident(&mut self) -> ast::Ident {
        self.save_span();
        match self.parser.parse_ident() {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.message());
                ast::Ident::with_empty_ctxt(self.builder.name("error"))
            }
        }
    }

    fn get_literal(&mut self) -> ast::Lit {
        self.save_span();
        let lit = match self.parser.parse_lit() {
            Ok(i) => { i }
            Err(e) => {
                // place the error
                self.set_err(e.message());
                // make a "default" to be ignored
                self.builder.lit().usize(0).unwrap()
            }
        };

        lit
    }


    //
    // value reading
    //

    pub fn parse_address(&mut self) -> u32 {
        let lit = self.get_literal().node;
        match parse_uint!(u32, lit) {
            Ok(i) => { i },
            Err(e) => {
                self.set_err(e.as_str());
                u32::max_value()
            }
        }
    }

    pub fn parse_lit_string(&mut self) -> String {
        self.get_ident().name.as_str().to_string()
    }

    //
    // register metadata
    //

    pub fn parse_reg_width(&mut self) -> common::RegisterWidth {
        match self.parse_lit_string().as_str() {
            "r8" => { common::RegisterWidth::R8 },
            "r16" => { common::RegisterWidth::R16 },
            "r32" => { common::RegisterWidth::R32 },
            "r64" => { common::RegisterWidth::R64 },
            _ => {
                self.set_err("unknown register width keyword");
                common::RegisterWidth::Unknown
            }
        }
    }

    pub fn parse_reg_access(&mut self) -> common::RegisterPermissions {
        match self.parse_lit_string().as_str() {
            "ro" => { common::RegisterPermissions::ReadOnly },
            "wo" => { common::RegisterPermissions::WriteOnly },
            "wr" => { common::RegisterPermissions::ReadWrite },
            _ => {
                self.set_err("unknown register access keyword");
                common::RegisterPermissions::Unknown
            }
        }
    }


    //
    // passthrough
    //

    pub fn eat(&mut self, tok: &token::Token) -> bool {
        self.parser.eat(tok)
    }
}
