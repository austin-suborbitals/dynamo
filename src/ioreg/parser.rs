extern crate aster;

use syntax::parse::token;
use syntax::ext::quote::rt::DUMMY_SP;

use std::collections::BTreeMap;

use common::data;
use common::parser::StaticValue;
use ioreg::common;


//
// parser
//

/// Thin wrapper around the CommonParser so we can add ioreg-specific functions to it.
pub type Parser<'a> = ::common::parser::CommonParser<'a>;


impl<'a> Parser<'a> {
    //
    // block/segment parsing
    //

    /// Entry to parsing the entire ioreg!() expansion.
    pub fn parse_ioreg(&mut self) -> common::IOReg {
        let start_span = self.parser.span;

        // create the info struct we will parse into
        let mut result = common::IOReg{
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
                    self.parse_constants_block(&"".to_string(), &mut result.const_vals);
                }
                "doc_srcs" => { self.parse_doc_sources(&mut result.doc_srcs); }

                _ => { self.set_err(format!("unexpected block keyword '{}'", tok).as_str()); break; }
            }
        }

        // the rest of the macro should be segments
        while ! self.eat(&token::Token::Eof) {
            let seg_opt = self.parse_segment();
			if seg_opt.is_none() { break; }

			let seg = seg_opt.unwrap();
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
    pub fn parse_segment(&mut self) -> Option<common::Segment> {
        let start_span = self.parser.span;

        // get an address
        let addr = match self.parse_uint::<usize>() {
			Ok(v) => { v }
			Err(e) => {
				self.parser.span_err(start_span, format!("could not parse address: {}", e).as_str());
				return None;
			}
		};
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
                    self.parse_constants_block(&name, &mut val_defs);
                }
                _ => { self.set_err("unexpected block keyword"); }
            }
        }

        let reg = data::RegisterType::new(width, data::Address(addr.val), perms)
                .expect("could not create register");

        let mut result = common::Segment{
            name: name,
            reg: reg,
            const_vals: val_defs,
            partials: vec!(),
            span: start_span,
        };

        // loop until we hit our closing brace
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            match self.parse_partial(&mut result) {
                Ok(o) => { result.partials.push(o); }
                Err(e) => {
                    self.set_err(e);  // TODO: and early escape?
                }
            }
        }

        // expect the end of offset blocks
        self.expect_semi(); // ends with a semicolon
        Some(result)
    }


    /// Parses a register offset and index.
    ///
    /// Checks the range is not inverted, and width >= 1.
    pub fn parse_index(&mut self) -> Option<data::RegisterSlice> {
        let start_span = self.parser.span;
        let begin = match self.parse_uint::<u8>() {
            Ok(v) => { v }
            Err(e) => {
                self.set_err(e.as_str());
                return None; // abort
            }
        };

        let end = match self.eat(&token::Token::DotDot) {
            false => { begin }
            true => {
                match self.parse_uint::<u8>() {
                    Ok(v) => { v }
                    Err(e) => {
                        self.set_err(e.as_str());
                        return None; // abort
                    }
                }
            }
        };

        if end < begin {
            self.set_err("index ranges are inverted");
            return None;
        }

        Some(data::RegisterSlice::new((end-begin).val+1, begin.val, start_span))
    }


    /// Parses offsets into the given ioreg segment and all associated functions.
    ///
    /// The block being parsed are those of the form `1..5 => { some_func => [...]; other_func => [...]; };`.
    pub fn parse_partial(&mut self, seg: &mut common::Segment)
        -> Result<common::Partial, &'static str>
    {
        // parse the index width and begin offset
        // then format the address based on the index/width
        let mut reg_off = match self.parse_index() {
            Some(r) => { r }
            None => { return Err("could not parse index"); }
        };

        let adj_addr = seg.reg.addr() + reg_off.offset_in_bytes(); // move N bytes forward
        reg_off.offset -= reg_off.offset_in_bytes() * 8;              // offset (N bytes * 8) bits lower

        let reg_part = data::PartialType::from(adj_addr, reg_off).expect("could not create partial");
        let last_bit = reg_part.slice().offset + reg_part.slice().width;
        let last_byte = last_bit / 8;

        if last_byte > seg.reg.width() {
            self.set_err(format!("index ({}) + width ({}) = {} cannot fit in register of size {}",
                reg_part.slice().offset, reg_part.slice().width,
				last_bit, seg.reg.width()).as_str()
			);
			return Err("invalid register configuration");
        }

        // we expect a fat arrow and a curly brace to kick things off
        self.expect_fat_arrow();
        self.expect_open_curly();

        // loop (parsing function defs) until we hit our closing bracket
        let mut fns = BTreeMap::<String, common::FunctionDef>::new();
        while ! self.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            // get a name and a colon to start the definition
            let name = self.parse_ident_string();

            // check the function has not been registered
            if ! fns.contains_key(&name) {
                let func = self.parse_func_def(name.clone());
                if func.ty == common::FunctionType::Setter && seg.reg.perms().read_only() {
                    return Err("cannot create setter function on read only segment");
                }

                fns.insert(name, func);
            } else {
                return Err("duplicate function definition");
            }
        }

        Ok(common::Partial{
            reg: reg_part,
            functions: fns,
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
    pub fn parse_static_setter_values(&mut self, result: &mut common::FunctionDef) {
        // until the end of the values
        while ! self.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            // inspect the token we are currently considering
            match self.parser.token {
                // if we have a literal, we expect it to be an integral, so parse that
                token::Token::Literal(token::Lit::Integer(_), _) => {
                    match self.parse_uint::<usize>() {
                        Ok(i) => { result.values.push(common::FunctionValueType::Static(i, self.parser.prev_span)); }
                        Err(e) => {
                            self.set_err(e.as_str());
                            break;  // TODO: better return
                        }
                    };
                }

                // otherwise, we expect an ident to reference a defined variable
                token::Ident(_) => {
                    result.values.push(common::FunctionValueType::Reference(self.parse_ident_string(), self.parser.prev_span));
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
    pub fn parse_func_def(&mut self, name: String) -> common::FunctionDef {
        let span = self.parser.span.clone();

        self.expect_fat_arrow();                  // functions must be followed with fat arrow
        let setter_type = match self.curr_token() {
            &token::Token::OpenDelim(token::DelimToken::Paren) => { common::FunctionType::Setter }
            &token::Token::OpenDelim(token::DelimToken::Bracket) => { common::FunctionType::StaticSetter }
            _ => {
                self.set_fatal_err("expected an open bracket or paren"); // fatal error
                common::FunctionType::Setter // make the compiler happy -- should not get used
            }
        };
        self.parser.bump();

        let mut result = common::FunctionDef {
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
            common::FunctionType::StaticSetter => { self.parse_static_setter_values(&mut result); }
            _ => { self.set_fatal_err("unexpected function type"); }
        }

        result
    }



    //
    // register metadata
    //

    /// Parses the width of a register (i.e. r8, r16, r32) into an integral representation.
    pub fn parse_reg_width(&mut self) -> usize {
        match self.parse_ident_string().as_str() {
            "r8" => { 8 },
            "r16" => { 16 },
            "r32" => { 32 },
            "r64" => { 64 },
            _ => {
                self.set_err("unknown register width keyword");
                0
            }
        }
    }

    /// Parses the access permissions of a register (i.e. ro, rw, wo) into the internal representation common::RegisterPermissions.
    pub fn parse_reg_access(&mut self) -> data::RegisterPermissions {
        match self.parse_ident_string().as_str() {
            "ro" => { data::RegisterPermissions::ReadOnly },
            "wo" => { data::RegisterPermissions::WriteOnly },
            "rw" => { data::RegisterPermissions::ReadWrite },
            _ => {
                self.set_err("unknown register access keyword");
                data::RegisterPermissions::Unknown
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
