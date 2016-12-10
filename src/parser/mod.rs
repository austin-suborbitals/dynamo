#![macro_use]

extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::tokenstream;
use syntax::symbol;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::DUMMY_SP;
use syntax::parse::parser as rsparse;

use std;
use std::fmt;
use std::collections::BTreeMap;

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
        match $parser.curr_token() {
            &token::Token::Ident(i) => { i.name.as_str().to_string().clone() } // TODO: these coercions are gross
            _ => { $parser.set_err("detected an ident, but did not parse as an ident"); "".to_string() } // TODO: better default
        }
    }
}


/// Trait used in register/value sizing and conversion.
///
/// This is used typically the "select" unsigned numerics in generic functions.
/// Each type must implement ::max_value() and ::min_value() for that type.
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



/// Simple trait to support "safe" narrowing (via truncation) of a u32 to a u16 or u8.
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


/// Trait for types that can convert themselves into ast types.
///
/// The required ast types are ast::Ty and ast::Lit + ast::Arg (with the latter two represented as ast::Expr)
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



// TODO: 32bit limitation imposed here
#[derive(Clone)]
/// Tuple type representing a static value parsed from the input.
///
/// Certain value types can only be used in certain places/actions.
/// Each type carries the Span for which it was defined to properly place errors.
pub enum StaticValue {
    Int(i32, String, Span),
    Uint(u32, String, Span),
    Float(f32, symbol::Symbol, String, Span), // TODO: avoid carrying the symbol around
    Str(String, String, Span),
    Ident(String, ast::Ident, Span),
    Path(ast::Path, Span),
    Error(String, Span),
}
impl StaticValue {
    pub fn default_uint() -> StaticValue {
        StaticValue::Uint(0, "default_static_uint".to_string(), DUMMY_SP)
    }

    pub fn to_string(&self) -> String {
        match self {
            &StaticValue::Int(i, _, _) => { i.to_string() }
            &StaticValue::Uint(u, _, _) => { u.to_string() }
            &StaticValue::Float(f, _, _, _) => { f.to_string() }
            &StaticValue::Str(ref s, _, _) => { s.clone() }
            &StaticValue::Ident(ref s, _, _) => { s.clone() }
            &StaticValue::Path(ref p, _) => { format!("{:?}", p) }
            &StaticValue::Error(ref e, _) => { e.clone() }
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
              &StaticValue::Int(_, _, sp)
            | &StaticValue::Uint(_, _, sp)
            | &StaticValue::Float(_, _, _, sp)
            | &StaticValue::Str(_, _, sp)
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


/// The common parser functions used across this crate.
///
/// Essentially a thin wrapper around syntax::parse::parser::Parser, with helpers added for our shared syntax.
pub struct CommonParser<'a> {
    pub parser:     rsparse::Parser<'a>,
        builder:    aster::AstBuilder,

    // state
    pub begin_segment:  Span,
}

impl<'a> CommonParser<'a> {
    /// Create a parser from the parsing context and token tree we wish to parse.
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[tokenstream::TokenTree]) -> CommonParser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        CommonParser {
            parser: p,
            builder: aster::AstBuilder::new(),
            begin_segment: s,   // TODO: better initializer
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
        self.parser.span_err(self.begin_segment, err);
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
            Err(e) => { self.set_fatal_err(e.message()); false }
        }
    }

    /// Expect the current token to be an equal sign ('=')
    ///
    /// If the current token is not the expect token, a fatal error is set.
    pub fn expect_equal(&mut self) -> bool {
        match self.parser.expect(&token::Token::Eq) {
            Ok(_) => { true }
            Err(e) => { self.set_fatal_err(e.message()); false }
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
                self.set_err(e.message());
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
                self.set_err(e.message());
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

    /// Parse the current token (i.e. literal) as a unsigned integer.
    ///
    /// This function is essentially a sanity wrapper around `checked_parse_uint()`.
    /// We cannot genericize the return value, so it is returned as a u64 which should then be cast appropriately.
    ///
    /// **Note:** there should be changes to the return value coming shortly, if Rust supports it.
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
    /// Reads the parser's current token as an unsigned numeric literal, asserting it fits into T::max_value().
    ///
    /// If the current token is not a numeric literal or the parsed value does not fit within T::max_value() then an
    /// error is returned describing as such.
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

    /// Parses the parser's current token as a literal, and converts it to our StaticValue type.
    ///
    /// **Note:** Numeric literals are tokenized as u32, and as such, must fit within u32::max_value().
    /// **Note:** Signed integers are not yet supported.
    ///
    /// There are multiple error cases here, depending on the type of literal.
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
                                let conv = s.as_str().parse::<f32>();
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


    /// Parse an entire `constants => { ... };` block into the given hash map.
    ///
    /// If any constant fails to parse, a syntax error is placed and parsing is aborted.
    /// This can lead to further, somewhat confusing, syntax errors but it follows the Rust style.
    ///
    /// **NOTE:** this function takes a validator function as an argument in case any validation needs to happen. This can be a nop.
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
