extern crate syntax;
use syntax::ast;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::quote::rt::DUMMY_SP;

extern crate bitmap;
use self::bitmap::Bitmap;

use std::collections::BTreeMap;

use ::parser;
use ::mcu::common;

// TODO: check less than usize? not sure if or what we want/need to validate
fn validate_constant(_: &(), _: &::parser::StaticValue, _: &mut BTreeMap<String, parser::StaticValue>)
    -> Result<(), String>
{
    Ok(())
}


pub type Parser<'a> = parser::CommonParser<'a>;

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> common::McuInfo {
        let mut result = common::McuInfo::default();
        result.span = self.parser.span;

        match self.curr_token() {
            &token::Token::Ident(n) => {
                if n.to_string() == "no_static" {
                    self.parser.bump();
                    self.expect_semi();
                    result.no_static = true;
                }
            }
            _ => {}
        }

        // parse the mcu name
        self.expect_ident_value("name");
        self.expect_fat_arrow();
        result.name = self.parse_ident_string();
        self.expect_semi();

        // parse the various portions of he mcu def
        while is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            let span = self.parser.span;
            match tok.as_str() {
                "constants" => {
                    self.parse_constants_block(
                        &"".to_string(), &mut result.constants, validate_constant, &()
                    );
                }
                "externs" => {
                    self.parse_externs_block(&mut result.externs);
                }
                "link_script" => {
                    self.parse_link_script(&mut result);
                }
                "doc_srcs" => {
                    self.parse_doc_sources(&"".to_string(), &mut result.docs);
                }
                "interrupts" => {
                    result.interrupts.span = self.parser.span.clone();
                    self.parse_interrupts(&mut result.interrupts);
                }
                "stack" => {
                    result.stack.span = self.parser.span.clone();
                    self.assert_keyword_preamble("stack");
                    self.parse_stack(&mut result.stack, &result.constants);
                }
                "data" => {
                    result.data.span = self.parser.span.clone();
                    self.assert_keyword_preamble("data");
                    self.parse_data(&mut result.data, &result.constants);
                }
                "heap" => {
                    result.heap.span = self.parser.span.clone();
                    self.assert_keyword_preamble("heap");
                    self.parse_heap(&mut result.heap, &result.constants);
                }
                "peripherals" => {
                    self.assert_keyword_preamble("peripherals");
                    self.parse_peripherals(&mut result.peripherals, &result.constants);
                }
                _ => {
                    self.parser.span_fatal(span, format!("unexpected block keyword '{}'", tok).as_str()).emit();
                    break;
                }
            }
        }

        result
    }

    pub fn parse_link_script(&mut self, into: &mut common::McuInfo) {
        self.expect_ident_value("link_script");
        self.expect_fat_arrow();

        // read the literal and assert string
        let doc_src = self.parse_constant_literal(&format!("{}_link_script", into.name));
        match doc_src {
            parser::StaticValue::Str(v, _, _) => { into.link_script = v; }
            _ => { self.set_err_last("expected a string literal"); return; }
        }

        self.expect_semi();
    }

    pub fn parse_externs_block(&mut self, into: &mut BTreeMap<String, (ast::TyKind, Span)>) {
        self.expect_ident_value("externs");
        self.expect_fat_arrow();
        self.expect_open_curly();

        while ! self.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            let span = self.parser.span;
            let name = self.parse_ident_string();
            self.expect_colon();
            let ty = self.parser.parse_ty_path().expect(format!("could not parse extern type for '{}'", name).as_str());
            into.insert(name, (ty, span));
            self.expect_semi();
        }
        self.expect_semi();
    }

    pub fn parse_interrupts(&mut self, into: &mut common::InterruptsInfo) {
        self.expect_ident_value("interrupts");
        self.expect_fat_arrow();

        // parse interrupt count
        self.expect_open_bracket();
        into.total_ints = self.parse_uint::<u8>() as u8;
        self.expect_close_bracket();

        // parse link section
        let exp_at = self.parser.expect(&token::Token::At);
        if exp_at.is_err() { exp_at.err().unwrap().emit(); }
        let exp_dot = self.parser.expect(&token::Token::Dot);
        if exp_dot.is_err() { exp_dot.err().unwrap().emit(); }
        into.link_location = self.parse_ident_string();

        // make a bitmap to make sure we set an interrupt only once
        let mut set_ints: Bitmap<_, bitmap::DynamicSize> =
            Bitmap::from_storage(into.total_ints as usize, 1 as usize, vec![0; ((into.total_ints/8)+1) as usize])
                .expect("could not create bitmap");

        // read block
        self.expect_open_curly();
        while ! self.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            let span = self.parser.span.clone();
            let range = self.parse_index_or_range();
            self.expect_fat_arrow();
            for i in range.begin..range.end+1 {
                if set_ints.get(i).expect("error getting bitmap index") == 1 {
                    self.parser.span_fatal(span, "a value for this interrupt has already been set -- the ranges probably overlap")
                        .emit();
                }
                set_ints.set(i, 1);
            }

            let sp = self.parser.span.clone();
            let fn_ident = match self.curr_token() {
                &token::Token::Ident(id) => {
                    parser::StaticValue::Ident(id.name.to_string().clone(), self.get_ident(), sp)
                }
                &token::Token::ModSep => {
                    parser::StaticValue::Path(self.get_type_path(), sp)
                }
                _ => {
                    self.set_fatal_err("expected a literal or ident");
                    parser::StaticValue::Uint(0, "0".to_string(), DUMMY_SP)
                }
            };

            into.ints.push((range, fn_ident));
            self.expect_semi();
        }
        self.expect_semi();
    }

    pub fn parse_stack(&mut self, into: &mut common::StackInfo, consts: &BTreeMap<String, parser::StaticValue>) {
        // parse base
        self.expect_ident_value("base");
        self.expect_fat_arrow();
        into.base = self.parse_lit_or_ident("stack_base", consts);
        self.expect_semi();

        // parse limit
        self.expect_ident_value("limit");
        self.expect_fat_arrow();
        into.limit = self.parse_lit_or_ident("stack_limit", consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    pub fn parse_data(&mut self, into: &mut common::DataInfo, consts: &BTreeMap<String, parser::StaticValue>) {
        // parse src
        self.expect_ident_value("src_begin");
        self.expect_fat_arrow();
        into.src_begin = self.parse_lit_or_ident("data_src_begin", consts);
        self.expect_semi();

        // parse dest_begin
        self.expect_ident_value("src_end");
        self.expect_fat_arrow();
        into.src_end = self.parse_lit_or_ident("data_src_end", consts);
        self.expect_semi();

        // parse dest_end
        self.expect_ident_value("dest");
        self.expect_fat_arrow();
        into.dest = self.parse_lit_or_ident("data_dest", consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    pub fn parse_heap(&mut self, into: &mut common::HeapInfo, consts: &BTreeMap<String, parser::StaticValue>) {
        // parse base
        self.expect_ident_value("base");
        self.expect_fat_arrow();
        into.base = self.parse_lit_or_ident("heap_base", consts);
        self.expect_semi();

        // parse limit
        self.expect_ident_value("limit");
        self.expect_fat_arrow();
        into.limit = self.parse_lit_or_ident("heap_limit", consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    pub fn parse_peripherals(&mut self, into: &mut Vec<common::PeripheralInfo>, consts: &BTreeMap<String, parser::StaticValue>) {
        while ! self.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            let sp = self.parser.span;
            let name = self.parse_ident_string();
            self.expect_fat_arrow();
            let periph = self.parser.parse_ty_path().expect(format!("could not parse type path for peripheral {}", name).as_str());
            let exp_at = self.parser.expect(&token::Token::At);
            if exp_at.is_err() { exp_at.err().unwrap().emit(); }

            let addr = self.parse_lit_or_ident(format!("{}_addr", name).as_str(), consts);
            into.push(common::PeripheralInfo{name: name, path: periph, ptr: addr, span: sp});
            self.expect_semi();
        }
        self.expect_semi();
    }

    //
    // helpers
    //

    pub fn assert_keyword_preamble(&mut self, expect: &str) {
        self.expect_ident_value(expect);
        self.expect_fat_arrow();
        self.expect_open_curly();
    }

    // NOTE: will return a literal if the ident is an internal constant
    pub fn parse_lit_or_ident(&mut self, name: &str, consts: &BTreeMap<String, parser::StaticValue>) -> parser::StaticValue {
        let sp = self.parser.span.clone();
        match self.curr_token() {
            &token::Token::Ident(id) => {
                if consts.contains_key(&id.name.to_string()) {
                    self.parser.bump(); // did not actually read anything
                    (*consts.get(&id.name.to_string()).expect("could not get constant from const map")).clone()
                } else {
                    parser::StaticValue::Ident(id.name.to_string().clone(), self.get_ident(), sp)
                }
            }
            &token::Token::Literal(_, _) => {
                parser::StaticValue::Uint(self.parse_uint::<u32>() as u32, name.to_string(), sp)
            }
            _ => {
                self.set_fatal_err("expected a literal or ident");
                parser::StaticValue::Uint(0, "0".to_string(), DUMMY_SP)
            }
        }
    }

    pub fn parse_index_or_range(&mut self) -> common::RangeInfo {
        let start = self.parse_uint::<u8>();
        let mut end = start;

        if self.parser.eat(&token::Token::DotDot) {
            end = self.parse_uint::<u8>();
            if end < start {
                self.set_fatal_err_last("range indices are inverted");
            }
        }

        common::RangeInfo{begin:start as usize, end:end as usize}
    }
}
