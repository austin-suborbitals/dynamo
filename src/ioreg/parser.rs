extern crate aster;

use syntax::ast;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::parse::parser as rsparse;

use std;
use std::collections::HashMap;

use ::ioreg::common;


pub trait IsNumeric<T> {
    fn max_value() -> T;
    fn min_value() -> T;
}

impl IsNumeric<u8> for u8 {
    fn max_value() -> u8 { u8::max_value() }
    fn min_value() -> u8 { u8::min_value() }
}
impl IsNumeric<u16> for u16 {
    fn max_value() -> u16 { u16::max_value() }
    fn min_value() -> u16 { u16::min_value() }
}
impl IsNumeric<u32> for u32 {
    fn max_value() -> u32 { u32::max_value() }
    fn min_value() -> u32 { u32::min_value() }
}
impl IsNumeric<f32> for f32 {
    fn max_value() -> f32 { std::f32::MAX }
    fn min_value() -> f32 { std::f32::MIN }
}

macro_rules! read_uint_for_register {
    ($parser:ident, $width:ident) => {{
        match $width {
            &common::RegisterWidth::R8 =>    { $parser.checked_parse_uint::<u8>() }
            &common::RegisterWidth::R16 =>   { $parser.checked_parse_uint::<u16>() }
            &common::RegisterWidth::R32 =>   { $parser.checked_parse_uint::<u32>() }
            &common::RegisterWidth::Unknown => {
                Err("cannot read value for register of unspecified size".to_string())
            }
        }
    }}
}

// determine if a given common::StaticValue can fit in the needed register
fn fits_into(val: &common::StaticValue, width: &common::RegisterWidth) -> bool {
    match val {
        &common::StaticValue::Error(_) => { false }
        &common::StaticValue::Int(i,_) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (i8::max_value() as i32) }
                &common::RegisterWidth::R16 => { i <= (i16::max_value() as i32) }
                &common::RegisterWidth::R32 => { i <= (i32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &common::StaticValue::Uint(i, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (u8::max_value() as u32) }
                &common::RegisterWidth::R16 => { i <= (u16::max_value() as u32) }
                &common::RegisterWidth::R32 => { i <= (u32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &common::StaticValue::Float(f, _, _) => {
            match width {
                &common::RegisterWidth::R8 | &common::RegisterWidth::R16 => { false } // TODO: what to do about f8 and f16
                &common::RegisterWidth::R32 => { f <= std::f32::MAX }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &common::StaticValue::Str(_, _) => { true } // TODO: this is because we "don't care" but perhaps we should?
    }
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
        let got = self.parse_ident_string();
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
                self.set_err(e.message());
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
    pub fn parse_index(&mut self) -> common::IoRegOffsetIndexInfo {
        let begin = self.parse_uint::<u8>() as u8;
        if begin == u8::max_value() {
            self.set_err("detected error while parsing index");
            return common::IoRegOffsetIndexInfo{offset: 0, width: 0};
        }

        let mut end = begin;
        if self.eat(&token::Token::DotDot) {
            end = self.parse_uint::<u8>() as u8;
            if end < begin {
                self.set_err("index ranges are inverted");
                return common::IoRegOffsetIndexInfo{offset: 0, width: 0};
            } else if end == begin {
                self.set_err("this should not be a range. indices are equal");
                return common::IoRegOffsetIndexInfo{offset: 0, width: 0};
            }
        }

        // get the width of the region
        let mut w = if begin == end { 1 } else { end - begin };

        // if we started by a zero-index, and it is a range, add 1 to fix 0-indexing
        if begin == 0 && begin != end { w += 1; }

        return common::IoRegOffsetIndexInfo{
            offset: begin,
            width: w,
        };
    }

    pub fn parse_uint<T: IsNumeric<T> + From<T>>(&mut self) -> u64
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
    // TODO: use the new common::Narrow trait
    pub fn checked_parse_uint<T: IsNumeric<T> + From<T>>(&mut self) -> Result<u64, String>
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

    pub fn parse_ident_string(&mut self) -> String {
        self.get_ident().name.as_str().to_string()
    }


    fn parse_constant_literal(&mut self, name: &String) -> common::StaticValue {
        self.save_span();
        match self.curr_token() {
            // parse an integer literal value
            &token::Token::Literal(token::Lit::Integer(_), _) => {
                match self.checked_parse_uint::<u32>() {
                    Ok(i) => {
                        return common::StaticValue::Uint(i as u32, name.clone());
                    }
                    Err(e) => {
                        return common::StaticValue::Error(e);
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
                                    return common::StaticValue::Error(conv.unwrap_err().to_string());
                                }
    
                                return common::StaticValue::Float(conv.unwrap(), s, name.clone());
                            }
                            _ => { return common::StaticValue::Error("could not be parsed as a float".to_string()); }
                        }
                    }
                    Err(e) => { return common::StaticValue::Error(e); }
                }
            }
    
            // parse a string literal value
            &token::Token::Literal(token::Lit::Str_(_), _) => {
                match self.get_literal() {
                    Ok(l) => {
                        match l.node {
                            ast::LitKind::Str(s, _) => {
                                return common::StaticValue::Str(s.to_string(), name.clone());
                            }
                            _ => { return common::StaticValue::Error("expected a string literal".to_string()); }
                        }
                    }
                    Err(e) => {
                        return common::StaticValue::Error(e);
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
                return common::StaticValue::Error("unexpected token. expected a static literal".to_string());
            }
        }
    }


    //
    // parse entire blocks/statements
    //


    // parse an entire `constants => { ... }` block into the given hash map
    pub fn parse_constants_block(
        &mut self, prefix: &String, into: &mut HashMap<String, common::StaticValue>, width: &common::RegisterWidth
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
                common::StaticValue::Error(e) => { self.set_err(e.as_str()); }
                // otherwise, push the value
                _ => {
                    if fits_into(&parsed_val, width) {
                        if into.contains_key(&const_name) {
                            self.set_err("duplicate constant definition");
                        } else {
                            into.insert(const_name.clone(), parsed_val);
                        }
                    } else {
                        self.set_err(format!("given literal does not fit into register size {:?}", width).as_str());
                    }
                }
            }
            self.expect_semi();               // expect a terminating semicolon for the value def
        }
        self.expect_semi();                   // expect a terminating semicolon for the constants block
    }


    // parse a function definition from the name to the semicolon after the argument values
    pub fn parse_func_def(&mut self, name: String, width: &common::RegisterWidth) -> common::IoRegFuncDef {
        self.expect_fat_arrow();                  // functions must be followed with fat arrow
        self.expect_open_bracket();               // expect a series of values

        let span = self.curr_span.clone();
    
        // until the end of the values
        let mut vals: Vec<common::FunctionValueType> = vec!();
        while ! self.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            // inspect the token we are currently considering
            match self.raw_parser().token {
                // if we have a literal, we expect it to be an integral, so parse that
                token::Token::Literal(token::Lit::Integer(_), _) => {  // TODO: consider float
                    match read_uint_for_register!(self, width) {
                        Ok(i) => { vals.push(common::FunctionValueType::Static(i as u32)); }
                        Err(e) => {
                            self.set_err(e.as_str());
                            break;  // TODO: better return
                        }
                    };
                }
    
                // otherwise, we expect an ident to reference a defined variable
                token::Ident(_) => {
                    vals.push(common::FunctionValueType::Reference(self.parse_ident_string()));
                }
    
                // consider everything else an error
                _ => {
                    self.set_err("unexepected token type"); // TODO: better error message
                }
            }
    
    
            // TODO: this currently allows [,,,,]
            self.eat(&token::Token::Comma);       // skip a comma if there is one
        }
        self.expect_semi();                       // expect a close to the definition
    
        common::IoRegFuncDef{
            name: name,
            values: vals,
            ty: common::FunctionType::Setter,
            span: span,
        }
    }



    //
    // register metadata
    //

    pub fn parse_reg_width(&mut self) -> common::RegisterWidth {
        match self.parse_ident_string().as_str() {
            "r8" => { common::RegisterWidth::R8 },
            "r16" => { common::RegisterWidth::R16 },
            "r32" => { common::RegisterWidth::R32 },
            _ => {
                self.set_err("unknown register width keyword");
                common::RegisterWidth::Unknown
            }
        }
    }

    pub fn parse_reg_access(&mut self) -> common::RegisterPermissions {
        match self.parse_ident_string().as_str() {
            "ro" => { common::RegisterPermissions::ReadOnly },
            "wo" => { common::RegisterPermissions::WriteOnly },
            "rw" => { common::RegisterPermissions::ReadWrite },
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

    pub fn curr_token(&self) -> &token::Token {
        &self.parser.token
    }
}
