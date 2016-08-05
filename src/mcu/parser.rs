extern crate syntax;

use syntax::parse::token;

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

        // parse doc block and constants
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
                _ => { self.set_err(format!("unexpected block keyword '{}'", tok).as_str()); break; }
            }
        }

        result
    }
}
