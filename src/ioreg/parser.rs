use syntax::ast;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::parse::parser as rsparse;

extern crate aster;

use super::common as common;


pub trait IsIntegral<T> {
    fn max_value() -> T;
    fn min_value() -> T;
}

impl IsIntegral<u8> for u8 {
    fn max_value() -> u8 { u8::max_value() }
    fn min_value() -> u8 { u8::min_value() }
}
impl IsIntegral<u16> for u16 {
    fn max_value() -> u16 { u16::max_value() }
    fn min_value() -> u16 { u16::min_value() }
}
impl IsIntegral<u32> for u32 {
    fn max_value() -> u32 { u32::max_value() }
    fn min_value() -> u32 { u32::min_value() }
}
impl IsIntegral<u64> for u64 {
    fn max_value() -> u64 { u64::max_value() }
    fn min_value() -> u64 { u64::min_value() }
}


pub struct Parser<'a> {
    pub parser:     rsparse::Parser<'a>,
        builder:    aster::AstBuilder,  // used for default value construction

    // state
    pub curr_span:      Span,
    pub begin_segment:  Span,
}


impl<'a> Parser<'a> {
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[ast::TokenTree]) -> Parser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        let result = Parser {
            parser: p,
            builder: aster::AstBuilder::new(),
            curr_span: s,
            begin_segment: s,   // TODO: better initializer
        };
        result.parser.abort_if_errors();
        result
    }

    pub fn set_err(&mut self, err: &str) {
        self.parser.span_err(self.curr_span, err);
    }

    pub fn set_segment_err(&mut self, err: &str) {
        self.parser.span_err(self.begin_segment, err);
    }

    pub fn raw_parser(&self) -> &rsparse::Parser { &self.parser }


    //
    // state saving
    //

    fn save_span(&mut self) {
        self.curr_span = self.parser.span.clone();
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

    pub fn expect_semi(&mut self) -> bool {
        self.save_span();
        match self.parser.expect(&token::Token::Semi) {
            Ok(_) => { true }
            Err(e) => { self.set_err(e.message()); false }
        }
    }

    pub fn expect_equal(&mut self) -> bool {
        self.save_span();
        match self.parser.expect(&token::Token::Eq) {
            Ok(_) => { true }
            Err(e) => { self.set_err(e.message()); false }
        }
    }

    pub fn expect_fat_arrow(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::FatArrow) {
            true => {true }
            false => { self.set_err("expected a fat arrow (=>)"); false }
        }
    }

    pub fn expect_colon(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::Colon) {
            true => { true }
            false => { self.set_err("expected a colon"); false }
        }
    }

    pub fn expect_open_curly(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Brace)) {
            true => { true }
            false => { self.set_err("expected an opening curly brace"); false }
        }
    }

    pub fn expect_close_curly(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            true => { false }
            false => { self.set_err("expected a closing curly brace"); false }
        }
    }

    pub fn expect_open_bracket(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_err("expected an opening bracket"); false }
        }
    }

    pub fn expect_close_bracket(&mut self) -> bool {
        self.save_span();
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_err("expected a closing bracket"); false }
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

    pub fn get_literal(&mut self) -> Result<ast::Lit, String> {
        self.save_span();
        match self.parser.parse_lit() {
            Ok(i) => {
                return Ok(i);
            }
            Err(e) => {
                return Err(e.message().to_string());
            }
        }
    }

    //
    // value reading
    //

    // parse and index span
    // can be a single number (i.e. 3) or a range (i.e. 1..5)
    // returned as a tuple of (begin, length)
    // TODO: Result<(u8,u8), _> perhaps?
    pub fn parse_index(&mut self) -> (u8, u8) {
        let begin = self.parse_uint::<u8>("u8") as u8;
        if begin == u8::max_value() {
            self.set_err("detected error while parsing index");
            return (0, 0);
        }

        let mut end = begin;
        if self.eat(&token::Token::DotDot) {
            end = self.parse_uint::<u8>("u8") as u8;
            if end < begin {
                self.set_err("index ranges are inverted");
                return (0, 0);
            } else if end == begin {
                self.set_err("this should not be a range. indices are equal");
                return (0, 0);
            }
        }
        return (begin, if begin == end { 1 } else { end - begin });
    }

    pub fn parse_uint<T: IsIntegral<T> + From<T>>(&mut self, desc: &'static str) -> u64
        where u64: From<T> {

        match self.checked_parse_uint::<T>(desc) {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.as_str());
                u64::max_value()
            }
        }
    }

    // TODO: why can I not cast u64 to T where T is uint < u64
    pub fn checked_parse_uint<T: IsIntegral<T> + From<T>>(&mut self, desc: &'static str) -> Result<u64, String>
        where u64: From<T> {

        let lit = match self.get_literal() {
            Ok(i) => { i.node }
            Err(e) => { return Err(e); }
        };
        if let ast::LitKind::Int(value, ty) = lit {
            match ty {
                ast::LitIntType::Unsigned(_) | ast::LitIntType::Unsuffixed => {
                    // some unsigned or unspecified int
                    if value <= (u64::from(T::max_value())) { // TODO: better assertion
                        return Ok(value);
                    } else {
                        return Err(format!("found value is larger than {}", desc));
                    }
                }
                // everything else
                _ => {
                    return Err(format!("expected {}, but parsed {:?}", desc, ty));
                }
            }
        } else {
            return Err(format!("expected a {}", desc));
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
        let r = self.parser.eat(tok);
        if ! r { self.parser.expected_tokens.pop(); }
        r
    }
}
