#![macro_use]

use std;
use std::fmt;
use std::collections::BTreeMap;

use syntax::ast;
use syntax::ptr;
use syntax::tokenstream;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::DUMMY_SP;
use syntax::parse::parser as rsparse;

// extern 

extern crate aster;
extern crate num_traits;

use self::num_traits::{PrimInt, ToPrimitive};


// local

use ::common::data;


/// Simple boolean check on whether the Parser's current token is a token::Ident.
macro_rules! is_ident {
    ($val:expr) => {
        match $val {
            &token::Token::Ident(_) => { true }
            _ => { false }
        }
    }
}

/// Get the current ident as a string, otherwise set a parser error.
macro_rules! extract_ident_name {
    ($parser:ident) => {
        match $parser.parser.token {
            token::Token::Ident(i) => { i.name.as_str().to_string().clone() } // TODO: these coercions are gross
            _ => { $parser.set_err("detected an ident, but did not parse as an ident"); "".to_string() } // TODO: better default
        }
    }
}


/// Trait for types that can convert themselves into ast types.
///
/// The required ast types are ast::Ty and ast::Lit + ast::Arg (with the latter two represented as ast::Expr)
pub trait ToAstType {
    fn to_type() -> ptr::P<ast::Ty>;
    fn to_lit(self) -> ptr::P<ast::Expr>;
    fn to_arg(self) -> ptr::P<ast::Expr>;
}

// TODO: do not require a new builder
impl ToAstType for u8 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u8() }
    fn to_lit(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(self) }
    fn to_arg(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(self) }
}
// TODO: do not require a new builder
impl ToAstType for u16 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u16() }
    fn to_lit(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(self) }
    fn to_arg(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(self) }
}
// TODO: do not require a new builder
impl ToAstType for u32 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u32() }
    fn to_lit(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(self) }
    fn to_arg(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(self) }
}
// TODO: do not require a new builder
impl ToAstType for u64 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u64() }
    fn to_lit(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u64(self) }
    fn to_arg(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u64(self) }
}
// TODO: do not require a new builder
impl ToAstType for usize {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().usize() }
    fn to_lit(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().usize(self) }
    fn to_arg(self) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().usize(self) }
}



/// Tuple type representing a static value parsed from the input.
///
/// Certain value types can only be used in certain places/actions.
/// Each type carries the Span for which it was defined to properly place errors.
#[derive(Clone)]
pub enum StaticValue {
    Uint(data::Unsigned),
    Str(String, Span),
    Ident(String, ast::Ident, Span), // TODO: remove String member
    Path(ast::Path, Span),
    Error(String, Span),
}
impl StaticValue {
    pub fn default_uint() -> StaticValue {
        StaticValue::Uint(data::Unsigned::nospan(0usize).expect("could not make 0usize into Unsigned"))
    }

    pub fn to_string(&self) -> String {
        match self {
            &StaticValue::Uint(u) => { u.val.to_string() }
            &StaticValue::Str(ref s, _) => { s.clone() }
            &StaticValue::Ident(ref s, _, _) => { s.clone() }
            &StaticValue::Path(ref p, _) => { format!("{:?}", p) }
            &StaticValue::Error(ref e, _) => { e.clone() }
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
              &StaticValue::Uint(u) => { u.span }

              &StaticValue::Str(_, sp)
            | &StaticValue::Ident(_, _, sp)
            | &StaticValue::Path(_, sp)
            | &StaticValue::Error(_, sp) =>
              { sp }
        }
    }
}

impl fmt::Debug for StaticValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &StaticValue::Uint(i) => { write!(f, "(uint) {}", i) }
            &StaticValue::Str(ref s, _) => { write!(f, "(str) {}", s) }
            &StaticValue::Ident(ref s, _, _) => { write!(f, "(ident) {}", s) }
            &StaticValue::Path(ref s, _) => { write!(f, "(path) {:?}", s) }
            &StaticValue::Error(ref e, _) => { write!(f, "(PARSER ERROR) {}", e) }
        }
    }
}


/// The common parser functions used across this crate.
///
/// Essentially a thin wrapper around syntax::parse::parser::Parser, with helpers added for our shared syntax.
pub struct CommonParser<'a> {
    pub parser:     rsparse::Parser<'a>,
        builder:    aster::AstBuilder,

    // state
    pub curr_segment_start:  Span,
}

impl<'a> CommonParser<'a> {
    /// Create a parser from the parsing context and token tree we wish to parse.
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[tokenstream::TokenTree]) -> CommonParser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        CommonParser {
            parser: p,
            builder: aster::AstBuilder::new(),
            curr_segment_start: s,
        }
    }

    /// Sets a syntax error on the span the parser's cursor is currently on.
    pub fn set_err(&mut self, err: &str) {
        self.parser.span_err(self.parser.span, err);
    }

    /// Sets a syntax error on the span **before** the parser's current token cursor.
    pub fn set_err_last(&mut self, err: &str) {
        self.parser.span_err(self.parser.prev_span, err);
    }

    /// Sets a fatal error on the parser's current token, and emits the diagnostic.
    pub fn set_fatal_err(&mut self, err: &str) {
        self.parser.span_fatal(self.parser.span, err).emit();
    }

    /// Sets a fatal error on the parser's previous token, and emits the diagnostic.
    pub fn set_fatal_err_last(&mut self, err: &str) {
        self.parser.span_fatal(self.parser.prev_span, err).emit();
    }

    /// Sets a syntax error on the span representing the current segment.
    ///
    /// **Note:** this will be moving to the ioreg parser shortly as this is generic, but missed in the split.
    pub fn set_segment_err(&mut self, err: &str) {
        self.parser.span_err(self.curr_segment_start, err);
    }


    //
    // assert and consume
    //

    /// Expects the current token to be an ast::Ident, and that the ident's name is equal to the given value.
    ///
    /// If the values are not equal, a fatal error is set.
    pub fn expect_ident_value(&mut self, expect: &str) {
        let got = self.parse_ident_string();
        if expect != got {
            self.set_fatal_err(format!("expected '{}' but found '{}'", expect, got).as_str());
        }
    }

    /// Expect the current token to be a semicolon
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_semi(&mut self) -> bool {
        match self.parser.expect(&token::Token::Semi) {
            Ok(_) => { true }
            Err(e) => { self.set_fatal_err(e.message().as_str()); false }
        }
    }

    /// Expect the current token to be an equal sign ('=')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_equal(&mut self) -> bool {
        match self.parser.expect(&token::Token::Eq) {
            Ok(_) => { true }
            Err(e) => { self.set_fatal_err(e.message().as_str()); false }
        }
    }

    /// Expect the current token to be a fat arrow ('=>')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_fat_arrow(&mut self) -> bool {
        match self.parser.eat(&token::FatArrow) {
            true => {true }
            false => { self.set_fatal_err("expected a fat arrow (=>)"); false }
        }
    }

    /// Expect the current token to be a colon
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_colon(&mut self) -> bool {
        match self.parser.eat(&token::Colon) {
            true => { true }
            false => { self.set_fatal_err("expected a colon"); false }
        }
    }

    /// Expect the current token to be a comma
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_comma(&mut self) -> bool {
        match self.parser.eat(&token::Comma) {
            true => { true }
            false => { self.set_fatal_err("expected a comma"); false }
        }
    }

    /// Expect the current token to be an opening paren ('('))
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_open_paren(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Paren)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening paren"); false }
        }
    }

    /// Expect the current token to be a closing paren (')')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_close_paren(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Paren)) {
            true => { false }
            false => { self.set_fatal_err("expected a closing paren"); false }
        }
    }

    /// Expect the current token to be an opening curly brace ('{')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_open_curly(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Brace)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening curly brace"); false }
        }
    }

    /// Expect the current token to be a closing curly brace ('}')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_close_curly(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            true => { false }
            false => { self.set_fatal_err("expected a closing curly brace"); false }
        }
    }

    /// Expect the current token to be an opening bracket ('[')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_open_bracket(&mut self) -> bool {
        match self.parser.eat(&token::OpenDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_fatal_err("expected an opening bracket"); false }
        }
    }

    /// Expect the current token to be a closing bracket (']')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_close_bracket(&mut self) -> bool {
        match self.parser.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            true => { true }
            false => { self.set_fatal_err("expected a closing bracket"); false }
        }
    }


    //
    // simple read
    //

    /// Get the parser's current token as an ast::Ident.
    ///
    /// If the current token is not an identifier, a non-fatal syntax error is set and a dummy is returned.
    pub fn get_ident(&mut self) -> ast::Ident {
        match self.parser.parse_ident() {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.message().as_str());
                self.builder.id("error")
            }
        }
    }

    /// Get the parser's current token(s) as an ast::Path.
    ///
    /// If the current token is not a path, a non-fatal syntax error is set and a dummy is returned.
    pub fn get_type_path(&mut self) -> ast::Path {
        match self.parser.parse_path(rsparse::PathStyle::Type) {
            Ok(i) => { i }
            Err(e) => {
                self.set_err(e.message().as_str());
                ast::Path::from_ident(DUMMY_SP, self.builder.id("error"))
            }
        }
    }

    /// Get the parser's current token as an ast::Lit.
    ///
    /// If the current token is not a literal, a non-fatal syntax error is set and an error is returned.
    pub fn get_literal(&mut self) -> Result<ast::Lit, String> {
        match self.parser.parse_lit() {
            Ok(i) => {
                Ok(i)
            }
            Err(e) => {
                self.set_err(e.message().as_str());
                Err(e.message())
            }
        }
    }


    //
    // numeric reads
    //

    /// Reads the parser's current token as an unsigned numeric literal, asserting it fits into T::max_value().
    ///
    /// If the current token is not a numeric literal an error is returned describing as such.
    pub fn parse_uint<T: PrimInt + ToPrimitive>(&mut self) -> Result<data::Unsigned, String> {
        let t_as_str = unsafe { std::intrinsics::type_name::<T>() };

        let sp = self.parser.span;
        let lit = match self.get_literal() {
            Ok(i) => { i.node }
            Err(e) => { return Err(e); }
        };


        if let ast::LitKind::Int(value, ty) = lit {
            match ty {
                ast::LitIntType::Unsigned(_) | ast::LitIntType::Unsuffixed => {
            		match (value as usize) < T::max_value().to_usize().unwrap() {
                    	true => { Ok(data::Unsigned::new(value as usize, sp)) }
            			false => {
            				Err(format!("value is too large to fit in {} bytes",
            					std::mem::size_of::<T>()*8
            				))
            			}
            		}
                }
                // everything else
                _ => {
                    Err(format!("expected {}, but parsed {:?}", t_as_str, ty))
                }
            }
        } else {
            Err(format!("expected a {}", t_as_str))
        }
    }

    /// Parses the parser's current token as a literal, and converts it to our StaticValue type.
    ///
    /// **Note:** Numeric literals are tokenized as usize (for the target platform).
    pub fn parse_constant_literal(&mut self) -> StaticValue {
        let curr_span = self.parser.span;
        match self.parser.token {
            // parse an integer literal value
            token::Token::Literal(token::Lit::Integer(_), _) => {
                match self.parse_uint::<usize>() {
                    Ok(i) => {
                        return StaticValue::Uint(i);
                    }
                    Err(e) => {
                        return StaticValue::Error(e, curr_span);
                    }
                }
            }
        
            // parse a string literal value
            token::Token::Literal(token::Lit::Str_(_), _) => {
                match self.get_literal() {
                    Ok(l) => {
                        match l.node {
                            ast::LitKind::Str(s, _) => {
                                return StaticValue::Str(s.to_string(), curr_span);
                            }
                            _ => { return StaticValue::Error("expected a string literal".to_string(), curr_span); }
                        }
                    }
                    Err(e) => {
                        return StaticValue::Error(e, curr_span);
                    }
                }
            }

            _ => {
                return StaticValue::Error("unexpected or unsupported token while parsing static literal".to_string(), curr_span);
            }
        }
    }




    //
    // string literal reads
    //

    /// Parses the current token as an ident, and returns the string value of the identifier.
    ///
    /// If the token is not an ident, a syntax error is placed and a dummy value returned.
    pub fn parse_ident_string(&mut self) -> String {
        self.get_ident().name.as_str().to_string()
    }


    //
    // block reading
    //

    /// Parses an entire `doc_srcs => [ ... ]` block into the given vector
    ///
    /// **Note:** trailing commas are not allowed.
    pub fn parse_doc_sources(&mut self, into: &mut Vec<String>) {
        // expect the opening syntax
        self.expect_ident_value("doc_srcs");
        self.expect_fat_arrow();
        self.expect_open_bracket();

        // until we hit the closing brace
        loop {
            // read the literal and assert string
            match self.parse_constant_literal() {
                StaticValue::Str(v, _) => { into.push(v); }
                _ => { self.set_err("expected a string literal"); return; }
            }

            if ! self.parser.eat(&token::Token::CloseDelim(token::DelimToken::Bracket)) {
                self.expect_comma();
            } else {
                break;
            }
        }
        self.expect_semi();                   // expect a terminating semicolon for the doc_srcs block
    }


    /// Parse an entire `constants => { ... };` block into the given hash map.
    ///
    /// If any constant fails to parse, a syntax error is placed.
    pub fn parse_constants_block(&mut self, prefix: &String, into: &mut BTreeMap<String, StaticValue>) {
        // expect the opening syntax
        self.expect_ident_value("constants");
        self.expect_fat_arrow();
        self.expect_open_curly();
        
        // until we hit the closing brace
        while ! self.parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            let mut const_name = self.parse_ident_string();
            if ! self.parser.eat(&token::Token::Eq) {
                self.set_err("expected a '=' after constant definition name");
                return; // TODO: should we just return here? would require func sig change
            }
    
            if prefix.len() > 0 {
                const_name = format!("{}_{}", prefix, const_name);
            }
        
            // get the value and add it to the list
            let parsed_val = self.parse_constant_literal();
            match parsed_val {
                // if error, set the error
                StaticValue::Error(e,_) => { self.set_err(e.as_str()); }
                // otherwise, push the value
                _ => {
                    if into.contains_key(&const_name) {
                        self.set_err("duplicate constant definition");
                    } else {
                        into.insert(const_name.clone(), parsed_val);
                    }
                }
            }
            self.expect_semi();               // expect a terminating semicolon for the value def
        }
        self.expect_semi();                   // expect a terminating semicolon for the constants block
    }
}
