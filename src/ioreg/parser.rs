extern crate aster;

use syntax::parse::token;
use syntax::ext::quote::rt::DUMMY_SP;

use std;
use std::collections::BTreeMap;

use ::parser::StaticValue;
use ::ioreg::common;


//
// helpers
//

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


/// Determine if a given parser::StaticValue can fit in the needed register
fn fits_into(val: &StaticValue, width: &common::RegisterWidth) -> bool {
    match val {
        &StaticValue::Error(_, _) => { false }
        &StaticValue::Ident(_, _, _) | &StaticValue::Path(_, _) => { false }
        &StaticValue::Int(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (i8::max_value() as i32) }
                &common::RegisterWidth::R16 => { i <= (i16::max_value() as i32) }
                &common::RegisterWidth::R32 => { i <= (i32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &StaticValue::Uint(i, _, _) => {
            match width {
                &common::RegisterWidth::R8 => { i <= (u8::max_value() as u32) }
                &common::RegisterWidth::R16 => { i <= (u16::max_value() as u32) }
                &common::RegisterWidth::R32 => { i <= (u32::max_value()) }
                &common::RegisterWidth::Unknown => { false }
            }
        }
        &StaticValue::Float(f, _, _, _) => {
            match width {
                // TODO: what to do about f8 and f16
                &common::RegisterWidth::R8 | &common::RegisterWidth::R16 => { false }
                &common::RegisterWidth::R32 => { f <= std::f32::MAX }
                &common::RegisterWidth::Unknown => { false }
            }
        }

        // TODO: this is because we "don't care" but perhaps we should?
        &StaticValue::Str(_, _, _) => { true }
    }
}

/// Validate parsed constants using `fits_into()`.
fn validate_constant(
    width: &common::RegisterWidth, val: &StaticValue, _: &mut BTreeMap<String, StaticValue>
)
    -> Result<(), String>
{
    if ! fits_into(&val, width) {
        Err(format!("given literal does not fit into register of size {:?}", width))
    } else {
        Ok(())
    }
}


//
// parser
//

/// Thin wrapper around the CommonParser so we can add ioreg-specific functions to it.
pub type Parser<'a> = ::parser::CommonParser<'a>;


impl<'a> Parser<'a> {
    //
    // block/segment parsing
    //

    /// Entry to parsing the entire ioreg!() expansion.
    pub fn parse_ioreg(&mut self) -> common::IoRegInfo {
        let start_span = self.parser.span;

        // create the info struct we will parse into
        let mut result = common::IoRegInfo{
            name: "".to_string(),
            doc_srcs: vec!(),
            segments: BTreeMap::new(),
            const_vals: BTreeMap::new(),
            init: common::InitInfo{defined:false, item:None, span: DUMMY_SP},
            span: start_span,
        };

        // check if we have a constants or doc_srcs definition block
        while is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            match tok.as_str() {
                "name" => {
                    // parse the name of the ioreg
                    self.expect_ident_value("name");
                    self.expect_fat_arrow();
                    result.name = self.parse_ident_string();
                    self.expect_semi();
                }
                "init" => {
                    let sp = self.parser.span.clone();
                    self.expect_ident_value("init");
                    self.expect_fat_arrow();
                    let init_item = self.parser.parse_impl_item();
                    self.expect_semi();

                    result.init = common::InitInfo{
                        defined: true,
                        item: match init_item {
                            Ok(i) => { Some(i) }
                            Err(mut e) => { e.emit(); None }
                        },
                        span: sp
                    };
                }
                "constants" => {
                    self.parse_constants_block(
                        &"".to_string(), &mut result.const_vals,
                        validate_constant, &common::RegisterWidth::R32
                    );
                }
                "doc_srcs" => { self.parse_doc_sources(&"".to_string(), &mut result.doc_srcs); }

                _ => { self.set_err(format!("unexpected block keyword '{}'", tok).as_str()); break; }
            }
        }

        // the rest of the macro should be segments
        while ! self.eat(&token::Token::Eof) {
            let seg = self.parse_segment();
            if ! result.segments.contains_key(seg.name.as_str()) {
                result.segments.insert(seg.name.clone(), seg);
            } else {
                self.set_segment_err(format!("duplicate section named '{}'", seg.name).as_str());
            }
        }

        result
    }


    /// Parses an entire segment block i.e. `0x1234 => some_segment r16 ro { ... };`.
    ///
    /// While "segments" are typically registers in and of themselves, they are more often than not accessed as a logical group.
    /// This grouping allows multiple related registers to be accessed as if they were one large (contiguous) register.
    pub fn parse_segment(&mut self) -> common::IoRegSegmentInfo {
        let start_span = self.parser.span;

        // get an address
        let addr = self.parse_uint::<u32>() as u32;
        self.begin_segment = self.parser.span;
        self.expect_fat_arrow();

        // gather metadata
        let name = self.parse_ident_string();
        let width = self.parse_reg_width();
        let perms = self.parse_reg_access();

        // expect the opening of offset blocks
        self.expect_open_curly();

        // see if we have a constants block, and if so, parse it
        let mut val_defs: BTreeMap<String, StaticValue> = BTreeMap::new();
        if is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            match tok.as_str() {
                "constants" => {
                    self.parse_constants_block(&name, &mut val_defs, validate_constant, &width);
                }
                _ => { self.set_err("unexpected block keyword"); }
            }
        }

        let mut result = common::IoRegSegmentInfo{
            name: name,
            address: addr,
            reg_width: width,
            access_perms: perms,
            const_vals: val_defs,
            offsets: vec!(),
            span: start_span,
        };

        // loop until we hit our closing brace
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            match self.parse_offset(&mut result) {
                Ok(o) => { result.push_offset(o); }
                Err(e) => {
                    self.set_err(e);  // TODO: and early escape?
                }
            }
        }

        // expect the end of offset blocks
        self.expect_semi();                       // ends with a semicolon
        result
    }


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


    /// Parses offsets into the given ioreg offset and all associated functions.
    ///
    /// The block being parsed are those of the form `1..5 => { some_func => [...]; other_func => [...]; };`.
    pub fn parse_offset(&mut self, seg: &mut common::IoRegSegmentInfo) -> Result<common::IoRegOffsetInfo, &'static str> {
        let start_span = self.parser.span;

        // parse the index width and begin offset
        let offset_index = self.parse_index();
        let last_bit = offset_index.offset + offset_index.width;
        if last_bit > seg.reg_width.as_u8() {
            self.set_err(format!("index ({}) + width ({}) = {} cannot fit in register of size {}",
                offset_index.offset, offset_index.width, last_bit, seg.reg_width.as_u8()).as_str());
        }

        // we expect a fat arrow and a curly brace to kick things off
        self.expect_fat_arrow();
        self.expect_open_curly();

        // loop (parsing function defs) until we hit our closing bracket
        let mut func_defs =  BTreeMap::<String, common::IoRegFuncDef>::new();
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            // get a name and a colon to start the definition
            let name = self.parse_ident_string();
            if ! func_defs.contains_key(&name) {    // check the function has not been registered TODO: check globally
                let func = self.parse_func_def(name.clone(), &seg.reg_width);
                if func.ty == common::FunctionType::Setter && seg.access_perms == common::RegisterPermissions::ReadOnly {
                    return Err("cannot create setter function on read only segment");
                }

                func_defs.insert(name, func);
            } else {
                return Err("duplicate function definition");
            }
        }

        Ok(common::IoRegOffsetInfo{
            index: offset_index,
            functions: func_defs,
            span: start_span,
        })

    }




    //
    // parse function parts
    //

    /// Parses a setter definition that uses static/literal values.
    ///
    /// The values used may be internal constants or literals.
    /// If an internal constant is specified, we lookup up the constant value and use it directly.
    /// If a constant (StaticValue) is found to not be numeric, a syntax error is placed.
    pub fn parse_static_setter_values(&mut self, result: &mut common::IoRegFuncDef, width: &common::RegisterWidth) {
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
