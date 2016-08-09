extern crate syntax;

use syntax::ast;
use syntax::parse::token;
use syntax::ext::quote::rt::DUMMY_SP;

use std::collections::BTreeMap;

#[macro_use]
use ::parser;
use ::mcu::common;

// TODO: check less than usize? not sure if or what we want/need to validate
fn validate_constant(ctx: &(), val: &::parser::StaticValue, into: &mut BTreeMap<String, parser::StaticValue>)
    -> Result<(), String>
{
    Ok(())
}


pub type Parser<'a> = parser::CommonParser<'a>;

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> common::McuInfo {
        let mut result = common::McuInfo::default();

        // parse the mcu name
        self.expect_ident_value("name");
        self.expect_fat_arrow();
        result.name = self.parse_ident_string();
        self.expect_semi();

        // parse the various portions of he mcu def
        while is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            match tok.as_str() {
                "constants" => {
                    self.parse_constants_block(
                        &"".to_string(), &mut result.constants, validate_constant, &()
                    );
                }
                "doc_srcs" => {
                    self.parse_doc_sources(&"".to_string(), &mut result.docs);
                }
                "interrupts" => {
                    self.parser.bump();
                    self.expect_fat_arrow();
                    self.parse_interrupts(&mut result.interrupts);
                }
                "stack" => {
                    self.assert_keyword_preamble();
                    self.parse_stack(&mut result.stack);
                }
                "data" => {
                    self.assert_keyword_preamble();
                    self.parse_data(&mut result.data);
                }
                "heap" => {
                    self.assert_keyword_preamble();
                    self.parse_heap(&mut result.heap);
                }
                "peripherals" => {
                    self.assert_keyword_preamble();
                    self.parse_peripherals(&mut result.peripherals);
                }
                _ => { self.set_err(format!("unexpected block keyword '{}'", tok).as_str()); break; }
            }
        }

        result
    }

    // parses `[num_interrupts] @ loc { ... }`
    pub fn parse_interrupts(&mut self, into: &mut Vec<Option<fn()>>) {
    }

    pub fn parse_stack(&mut self, into: &mut common::StackInfo) {
        self.expect_ident_value("base");
        self.expect_fat_arrow();

        // if literal: get usize lit
        // else: get ident

        // repeat for base
    }

    pub fn parse_data(&mut self, into: &mut common::DataInfo) {
    }

    pub fn parse_heap(&mut self, into: &mut common::HeapInfo) {
    }

    pub fn parse_peripherals(&mut self, into: &mut Vec<ast::Path>) {
    }

    //
    // helpers
    //

    pub fn assert_keyword_preamble(&mut self) {
        self.parser.bump();
        self.expect_fat_arrow();
        self.expect_open_curly();
    }

    pub fn parse_lit_or_ident(&mut self) -> parser::StaticValue {
        parser::StaticValue::Uint(0, "0".to_string(), DUMMY_SP)
    }

    pub fn parse_index_or_range(&mut self) -> common::RangeInfo {
        common::RangeInfo{begin:0, end:0}
    }
}
