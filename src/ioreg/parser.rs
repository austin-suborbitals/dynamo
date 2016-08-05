extern crate aster;

use syntax::parse::token;

use std;
use std::collections::BTreeMap;

use ::parser;
use ::ioreg::common;


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

// determine if a given parser::StaticValue can fit in the needed register
fn fits_into(val: &parser::StaticValue, width: &common::RegisterWidth) -> bool {
    match val {
        &parser::StaticValue::Error(_, _) => { false }
        &parser::StaticValue::Int(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (i8::max_value() as i32) }
                &common::RegisterWidth::R16 => { i <= (i16::max_value() as i32) }
                &common::RegisterWidth::R32 => { i <= (i32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &parser::StaticValue::Uint(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (u8::max_value() as u32) }
                &common::RegisterWidth::R16 => { i <= (u16::max_value() as u32) }
                &common::RegisterWidth::R32 => { i <= (u32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &parser::StaticValue::Float(f, _, _, _) => {
            match width {
                &common::RegisterWidth::R8 | &common::RegisterWidth::R16 => { false } // TODO: what to do about f8 and f16
                &common::RegisterWidth::R32 => { f <= std::f32::MAX }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &parser::StaticValue::Str(_, _, _) => { true } // TODO: this is because we "don't care" but perhaps we should?
    }
}



pub type Parser<'a> = parser::CommonParser<'a>;


impl<'a> Parser<'a> {

    // parse and index span
    // can be a single number (i.e. 3) or a range (i.e. 1..5)
    // returned as a tuple of (begin, length)
    // TODO: Result<(u8,u8), _> perhaps?
    pub fn parse_index(&mut self) -> common::IoRegOffsetIndexInfo {
        let start_span = self.curr_span;
        let begin = self.parse_uint::<u8>() as u8;
        if begin == u8::max_value() {
            self.set_err("detected error while parsing index");
            return common::IoRegOffsetIndexInfo{offset: 0, width: 0, span: start_span};
        }

        let mut end = begin;
        if self.eat(&token::Token::DotDot) {
            end = self.parse_uint::<u8>() as u8;
            if end < begin {
                self.set_err("index ranges are inverted");
                return common::IoRegOffsetIndexInfo{offset: 0, width: 0, span: start_span};
            } else if end == begin {
                self.set_err("this should not be a range. indices are equal");
                return common::IoRegOffsetIndexInfo{offset: 0, width: 0, span: start_span};
            }
        }

        return common::IoRegOffsetIndexInfo{
            offset: begin,
            width: (end-begin)+1, // add one to account for zero indexing
            span: start_span,
        };
    }

    //
    // parse entire blocks/statements
    //


    // parse an entire `constants => { ... }` block into the given hash map
    pub fn parse_constants_block(
        &mut self, prefix: &String, into: &mut BTreeMap<String, parser::StaticValue>, width: &common::RegisterWidth
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
                parser::StaticValue::Error(e,_) => { self.set_err(e.as_str()); }
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


    // parse an entire `doc_srcs => [ ... ]` block into the given vector
    pub fn parse_doc_sources(&mut self, prefix: &String, into: &mut Vec<String>) {
        // expect the opening syntax
        self.expect_ident_value("doc_srcs");
        self.expect_fat_arrow();
        self.expect_open_bracket();

        let mut doc_cnt: usize = 0;
        
        // until we hit the closing brace
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Bracket)) {
            // read the literal and assert string
            let doc_src = self.parse_constant_literal(&format!("{}_doc_{}", prefix, doc_cnt));
            match doc_src {
                parser::StaticValue::Str(v, _, _) => { into.push(v); doc_cnt += 1; }
                _ => { self.set_err("expected a string literal"); return; }
            }

            self.expect_comma();
        }
        self.expect_semi();                   // expect a terminating semicolon for the doc_srcs block
    }


    fn parse_static_setter_values(&mut self, result: &mut common::IoRegFuncDef, width: &common::RegisterWidth) {
        // until the end of the values
        while ! self.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            // inspect the token we are currently considering
            match self.raw_parser().token {
                // if we have a literal, we expect it to be an integral, so parse that
                token::Token::Literal(token::Lit::Integer(_), _) => {  // TODO: consider float
                    match read_uint_for_register!(self, width) {
                        Ok(i) => { result.values.push(common::FunctionValueType::Static(i as u32)); }
                        Err(e) => {
                            self.set_err(e.as_str());
                            break;  // TODO: better return
                        }
                    };
                }
    
                // otherwise, we expect an ident to reference a defined variable
                token::Ident(_) => {
                    result.values.push(common::FunctionValueType::Reference(self.parse_ident_string()));
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
    }

    // parse a function definition from the name to the semicolon after the argument values
    pub fn parse_func_def(&mut self, name: String, width: &common::RegisterWidth) -> common::IoRegFuncDef {
        let span = self.curr_span.clone();

        self.expect_fat_arrow();                  // functions must be followed with fat arrow
        let setter_type = match self.curr_token() {
            &token::Token::OpenDelim(token::DelimToken::Paren) => { common::FunctionType::Setter }
            &token::Token::OpenDelim(token::DelimToken::Bracket) => { common::FunctionType::StaticSetter }
            _ => {
                self.set_fatal_err("expected an open bracket or paren"); // fatal error
                common::FunctionType::Setter                             // make the compiler happy -- should not get used
            }
        };
        self.parser.bump();

        let mut result = common::IoRegFuncDef {
            name: name,
            values: vec!(),
            ty: setter_type,
            span: span,
        };

        match setter_type {
            common::FunctionType::Setter => {
                self.expect_close_paren();
                self.expect_semi();
            }
            common::FunctionType::StaticSetter => { self.parse_static_setter_values(&mut result, width); }
            _ => { self.set_fatal_err("unexpected function type"); }
        }

        result
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
