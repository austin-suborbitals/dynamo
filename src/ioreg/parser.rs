extern crate aster;

use syntax::parse::token;

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

/// Thin wrapper around the CommonParser so we can add ioreg-specific functions to it.
pub type Parser<'a> = parser::CommonParser<'a>;


impl<'a> Parser<'a> {
    /// Parses a register offset and index.
    ///
    /// Checks the range is not inverted, and width >= 1.
    pub fn parse_index(&mut self) -> common::IoRegOffsetIndexInfo {
        let start_span = self.parser.span;
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
    // parse function parts
    //

    /// Parses a setter definition that uses static/literal values.
    ///
    /// The values used may be internal constants or literals.
    /// If an internal constant is specified, we lookup up the constant value and use it directly.
    /// If a constant (StaticValue) is found to not be numeric, a syntax error is placed.
    fn parse_static_setter_values(&mut self, result: &mut common::IoRegFuncDef, width: &common::RegisterWidth) {
        // until the end of the values
        while ! self.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            // inspect the token we are currently considering
            match self.parser.token {
                // if we have a literal, we expect it to be an integral, so parse that
                token::Token::Literal(token::Lit::Integer(_), _) => {  // TODO: consider float
                    match read_uint_for_register!(self, width) {
                        Ok(i) => { result.values.push(common::FunctionValueType::Static(i as u32, self.parser.last_span)); }
                        Err(e) => {
                            self.set_err(e.as_str());
                            break;  // TODO: better return
                        }
                    };
                }
    
                // otherwise, we expect an ident to reference a defined variable
                token::Ident(_) => {
                    result.values.push(common::FunctionValueType::Reference(self.parse_ident_string(), self.parser.last_span));
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

    /// Parses a function definition on a register.
    ///
    /// Consumes `fn_name => [....];` or `fn_name => ();` depending on function type.
    pub fn parse_func_def(&mut self, name: String, width: &common::RegisterWidth) -> common::IoRegFuncDef {
        let span = self.parser.span.clone();

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

    /// Parses the width of a register (i.e. r8, r16, r32) into the internal representation common::RegisterWidth.
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

    /// Parses the access permissions of a register (i.e. ro, rw, wo) into the internal representation common::RegisterPermissions.
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

    /// Shortcut to self.parser.eat()
    pub fn eat(&mut self, tok: &token::Token) -> bool {
        let r = self.parser.eat(tok);
        if ! r { self.parser.expected_tokens.pop(); }
        r
    }

    /// Shortcut to self.parser.token
    pub fn curr_token(&self) -> &token::Token {
        &self.parser.token
    }
}
