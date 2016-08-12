#![macro_use]

extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::tokenstream;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::DUMMY_SP;
use syntax::parse::parser as rsparse;

use std;
use std::fmt;
use std::collections::BTreeMap;

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


pub trait HasMinMax<T> {
    fn max_value() -> T;
    fn min_value() -> T;
}

impl HasMinMax<u8> for u8 {
    fn max_value() -> u8 { u8::max_value() }
    fn min_value() -> u8 { u8::min_value() }
}
impl HasMinMax<u16> for u16 {
    fn max_value() -> u16 { u16::max_value() }
    fn min_value() -> u16 { u16::min_value() }
}
impl HasMinMax<u32> for u32 {
    fn max_value() -> u32 { u32::max_value() }
    fn min_value() -> u32 { u32::min_value() }
}
impl HasMinMax<f32> for f32 {
    fn max_value() -> f32 { std::f32::MAX }
    fn min_value() -> f32 { std::f32::MIN }
}



//
// narrow from u32 to u8 and u16
//
pub trait Narrow<T> {
    fn narrow(u: T) -> Self;
}
impl Narrow<u32> for u8 {
    fn narrow(u: u32) -> Self { (u & 0xFFFF) as u8 }
}
impl Narrow<u32> for u16 {
    fn narrow(u: u32) -> Self { (u & 0xFFFF) as u16 }
}
impl Narrow<u32> for u32 {
    fn narrow(u: u32) -> Self { u }
}



pub trait ToAstType<T> {
    fn to_type() -> ptr::P<ast::Ty>;
    fn to_lit(val: T) -> ptr::P<ast::Expr>;
    fn to_arg(val: T) -> ptr::P<ast::Expr>;
}

// TODO: do not require a new builder
impl ToAstType<u8> for u8 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u8() }
    fn to_lit(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
    fn to_arg(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
}
// TODO: do not require a new builder
impl ToAstType<u16> for u16 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u16() }
    fn to_lit(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
    fn to_arg(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
}
// TODO: do not require a new builder
impl ToAstType<u32> for u32 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u32() }
    fn to_lit(val: u32) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(val) }
    fn to_arg(val: u32) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(val) }
}



//
// static value type
//

// TODO: 32bit limitation imposed here
#[derive(Clone)]
pub enum StaticValue {
    Int(i32, String, Span),
    Uint(u32, String, Span),
    Float(f32, token::InternedString, String, Span), // TODO: avoid carrying the interned string around
    Str(String, String, Span),
    Ident(String, ast::Ident, Span),
    Path(ast::Path, Span),
    Error(String, Span),
}
impl StaticValue {
    pub fn default_uint() -> StaticValue {
        StaticValue::Uint(0, "default_static_uint".to_string(), DUMMY_SP)
    }
}

impl fmt::Debug for StaticValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &StaticValue::Int(i, ref n, _) => { write!(f, "({}:int) {}", n, i) }
            &StaticValue::Uint(i, ref n, _) => { write!(f, "({}:uint) {}", n, i) }
            &StaticValue::Float(i, _, ref n, _) => { write!(f, "({}:float) {}", n, i) }
            &StaticValue::Str(ref s, ref n, _) => { write!(f, "({}:str) {}", n, s) }
            &StaticValue::Ident(ref s, _, _) => { write!(f, "({}:ident)", s) }
            &StaticValue::Path(ref s, _) => { write!(f, "({:?}:path)", s) }
            &StaticValue::Error(ref e, _) => { write!(f, "(PARSER ERROR) {}", e) }
        }
    }
}




pub struct CommonParser<'a> {
    pub parser:     rsparse::Parser<'a>,
        builder:    aster::AstBuilder,

    // state
    pub begin_segment:  Span,
}

impl<'a> CommonParser<'a> {
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[tokenstream::TokenTree]) -> CommonParser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        CommonParser {
            parser: p,
            builder: aster::AstBuilder::new(),
            begin_segment: s,   // TODO: better initializer
        }
    }

    pub fn set_err(&mut self, err: &str) {
        self.parser.span_err(self.parser.span, err);
    }
    pub fn set_err_last(&mut self, err: &str) {
        self.parser.span_err(self.parser.last_span, err);
    }

    pub fn set_fatal_err(&mut self, err: &str) {
        self.parser.span_fatal(self.parser.span, err).emit();
    }
    pub fn set_fatal_err_last(&mut self, err: &str) {
        self.parser.span_fatal(self.parser.last_span, err).emit();
    }

    pub fn set_segment_err(&mut self, err: &str) {
        self.parser.span_err(self.begin_segment, err);
    }

    pub fn raw_parser(&self) -> &rsparse::Parser { &self.parser }


    //
    // assert and consume
    //

    pub fn expect_ident_value(&mut self, expect: &str) {
        let got = self.parse_ident_string();
        if expect != got {
            self.set_fatal_err(format!("expected '{}' but found '{}'", expect, got).as_str());
        }
    }

    pub fn expect_semi(&mut self) -> bool {
        match self.parser.expect(&token::Token::Semi) {
            Ok(_) => { true }
            Err(e) => { self.set_fatal_err(e.message()); false }
        }
    }

    pub fn expect_equal(&mut self) -> bool {
        match self.parser.expect(&token::Token::Eq) {
            Ok(_) => { true }
            Err(e) => { self.set_fatal_err(e.message()); false }
        }
    }

    pub fn expect_fat_arrow(&mut self) -> bool {
        match self.parser.eat(&token::FatArrow) {
            true => {true }
            false => { self.set_fatal_err("expected a fat arrow (=>)"); false }
        }
    }

    pub fn expect_colon(&mut self) -> bool {
        match self.parser.eat(&token::Colon) {
            true => { true }
            false => { self.set_fatal_err("expected a colon"); false }
        }
    }

    pub fn expect_comma(&mut self) -> bool {
        match self.parser.eat(&token::Comma) {
            true => { true }
            false => { self.set_fatal_err("expected a comma"); false }
        }
    }

    pub fn expect_open_paren(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Paren)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening paren"); false }
        }
    }

    pub fn expect_close_paren(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Paren)) {
            true => { false }
            false => { self.set_fatal_err("expected a closing paren"); false }
        }
    }

    pub fn expect_open_curly(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Brace)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening curly brace"); false }
        }
    }

    pub fn expect_close_curly(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            true => { false }
            false => { self.set_fatal_err("expected a closing curly brace"); false }
        }
    }

    pub fn expect_open_bracket(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening bracket"); false }
        }
    }

    pub fn expect_close_bracket(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_fatal_err("expected a closing bracket"); false }
        }
    }


    //
    // simple read
    //

    pub fn get_ident(&mut self) -> ast::Ident {
        match self.parser.parse_ident() {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.message());
                ast::Ident::with_empty_ctxt(self.builder.name("error"))
            }
        }
    }

    pub fn get_type_path(&mut self) -> ast::Path {
        match self.parser.parse_path(rsparse::PathStyle::Type) {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.message());
                ast::Path::from_ident(DUMMY_SP, aster::AstBuilder::new().id("error"))
            }
        }
    }

    pub fn get_literal(&mut self) -> Result<ast::Lit, String> {
        match self.parser.parse_lit() {
            Ok(i) => {
                return Ok(i);
            }
            Err(e) => {
                self.set_err(e.message());
                return Err(e.message().to_string());
            }
        }
    }


    //
    // numeric reads
    //

    pub fn parse_uint<T: HasMinMax<T> + From<T>>(&mut self) -> u64
        where u64: From<T> {

        match self.checked_parse_uint::<T>() {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.as_str());
                u64::max_value()
            }
        }
    }

    // TODO: why can I not cast u64 to T where T is uint < u64
    // TODO: use the new Narrow trait
    pub fn checked_parse_uint<T: HasMinMax<T> + From<T>>(&mut self) -> Result<u64, String>
        where u64: From<T> {

        let t_as_str = unsafe { std::intrinsics::type_name::<T>() };

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
                        return Err(format!("found value is larger than {}", t_as_str));
                    }
                }
                // everything else
                _ => {
                    return Err(format!("expected {}, but parsed {:?}", t_as_str, ty));
                }
            }
        } else {
            return Err(format!("expected a {}", t_as_str));
        }
    }


    pub fn parse_constant_literal(&mut self, name: &String) -> StaticValue {
        let curr_span = self.parser.span;
        match self.curr_token() {
            // parse an integer literal value
            &token::Token::Literal(token::Lit::Integer(_), _) => {
                match self.checked_parse_uint::<u32>() {
                    Ok(i) => {
                        return StaticValue::Uint(i as u32, name.clone(), curr_span);
                    }
                    Err(e) => {
                        return StaticValue::Error(e, curr_span);
                    }
                }
            }
        
            // parse a floating literal value
            &token::Token::Literal(token::Lit::Float(_), _) => {
                match self.get_literal() {
                    Ok(l) => {
                        match l.node {
                            // TODO: do we care about type? stored as f64....
                            ast::LitKind::Float(s, _) | ast::LitKind::FloatUnsuffixed(s) => {
                                let conv = s.parse::<f32>();
                                if conv.is_err() {
                                    return StaticValue::Error(conv.unwrap_err().to_string(), curr_span);
                                }
    
                                return StaticValue::Float(conv.unwrap(), s, name.clone(), curr_span);
                            }
                            _ => { return StaticValue::Error("could not be parsed as a float".to_string(), curr_span); }
                        }
                    }
                    Err(e) => { return StaticValue::Error(e, curr_span); }
                }
            }
    
            // parse a string literal value
            &token::Token::Literal(token::Lit::Str_(_), _) => {
                match self.get_literal() {
                    Ok(l) => {
                        match l.node {
                            ast::LitKind::Str(s, _) => {
                                return StaticValue::Str(s.to_string(), name.clone(), curr_span);
                            }
                            _ => { return StaticValue::Error("expected a string literal".to_string(), curr_span); }
                        }
                    }
                    Err(e) => {
                        return StaticValue::Error(e, curr_span);
                    }
                }
            }

            // TODO: we need to handle negative ints
            /*
            &token::Token::BinOp(token::BinOpToken::Minus) => {
                self.eat(&token::Token::BinOp(token::BinOpToken::Minus));
                return self.parse_constant_literal(name);
            }
            */

            _ => {
                return StaticValue::Error("unexpected token. expected a static literal".to_string(), curr_span);
            }
        }
    }




    //
    // string literal reads
    //

    pub fn parse_ident_string(&mut self) -> String {
        self.get_ident().name.as_str().to_string()
    }


    //
    // block reading
    //

    // parse an entire `doc_srcs => [ ... ]` block into the given vector
    pub fn parse_doc_sources(&mut self, prefix: &String, into: &mut Vec<String>) {
        // expect the opening syntax
        self.expect_ident_value("doc_srcs");
        self.expect_fat_arrow();
        self.expect_open_bracket();

        let mut doc_cnt: usize = 0;
        
        // until we hit the closing brace
        loop {
            // read the literal and assert string
            let doc_src = self.parse_constant_literal(&format!("{}_doc_{}", prefix, doc_cnt));
            match doc_src {
                StaticValue::Str(v, _, _) => { into.push(v); doc_cnt += 1; }
                _ => { self.set_err("expected a string literal"); return; }
            }

            if ! self.eat(&token::Token::CloseDelim(token::DelimToken::Bracket)) {
                self.expect_comma();
            } else {
                break;
            }
        }
        self.expect_semi();                   // expect a terminating semicolon for the doc_srcs block
    }


    // parse an entire `constants => { ... }` block into the given hash map
    pub fn parse_constants_block<T>(
        &mut self, prefix: &String, into: &mut BTreeMap<String, StaticValue>,
        validate: fn(&T,&StaticValue,&mut BTreeMap<String, StaticValue>) -> Result<(), String>,
        validate_ctx: &T
    ) {
        // expect the opening syntax
        self.expect_ident_value("constants");
        self.expect_fat_arrow();
        self.expect_open_curly();
        
        // until we hit the closing brace
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            let mut const_name = self.parse_ident_string();
            if ! self.eat(&token::Token::Eq) {
                self.set_err("expected a '=' after constant definition name");
                return; // TODO: should we just return here? would require func sig change
            }
    
            if prefix.len() > 0 {
                const_name = format!("{}_{}", prefix, const_name);
            }
        
            // get the value and add it to the list
            let parsed_val = self.parse_constant_literal(&const_name);
            match parsed_val {
                // if error, set the error
                StaticValue::Error(e,_) => { self.set_err(e.as_str()); }
                // otherwise, validate and push the value
                _ => {
                    match validate(validate_ctx, &parsed_val, into) {
                        Ok(_) => {
                            if into.contains_key(&const_name) {
                                self.set_err("duplicate constant definition");
                            } else {
                                into.insert(const_name.clone(), parsed_val);
                            }
                        }
                        Err(e) => {
                            self.set_err(e.as_str());
                        }
                    }
                }
            }
            self.expect_semi();               // expect a terminating semicolon for the value def
        }
        self.expect_semi();                   // expect a terminating semicolon for the constants block
    }
}
