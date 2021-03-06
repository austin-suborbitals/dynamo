extern crate syntax;
use syntax::ast;
use syntax::parse::token;
use syntax::parse::parser::PathStyle;
use syntax::codemap::Span;

extern crate bitmap;
use self::bitmap::Bitmap;

use std::collections::BTreeMap;

use ::common::data;
use ::common::parser;
use ::common::parser::StaticValue;
use ::mcu::common;

macro_rules! token_is_path {
    ($self_:ident) => {
        match $self_.curr_token() {
            &token::Token::ModSep => { true }
            _ => {
                $self_.parser.look_ahead(1, |ref t| {
                    match t {
                        &&token::Token::ModSep => { true }
                        _ => { false }
                    }
                })
            }
        }
    }
}

/// Extends the ::parser::CommonParser struct to add mcu-specific parsing functions.
///
/// Inherits the `from()` initializer function.
pub type Parser<'a> = parser::CommonParser<'a>;

impl<'a> Parser<'a> {
    /// Once the parser is created, use this function to consume the entire syntax of the mcu.
    ///
    /// For MCU parsing, blocks can be in any order. However, some things may depend on order of definition.
    pub fn parse(&mut self) -> common::McuInfo {
        let mut result = common::McuInfo::default();
        result.span = self.parser.span;

        // parse the various portions of he mcu def
        while is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            let span = self.parser.span;
            match tok.as_str() {
                "no_init" => {
                    result.no_init = true;
                    self.parser.bump();
                    self.expect_semi();
                }
                "name" => {
                    self.expect_ident_value("name");
                    self.expect_fat_arrow();
                    result.name = self.parse_ident_string();
                    self.expect_semi();
                }
                "constants" => {
                    self.parse_constants_block(&"".to_string(), &mut result.constants);
                }
                "externs" => {
                    self.parse_externs_block(&mut result.externs);
                }
                "link_script" => {
                    self.parse_link_script(&mut result);
                }
                "doc_srcs" => {
                    self.parse_doc_sources(&mut result.docs);
                }
                "interrupts" => {
                    result.interrupts.span = self.parser.span.clone();
                    self.parse_interrupts(&mut result.interrupts);
                }
                "nvic" => {
                    self.parse_nvic(&mut result.nvic);
                }
                "memory" => {
                    self.parse_memory(&mut result);
                }
                "peripherals" => {
                    self.assert_keyword_preamble("peripherals");
                    self.parse_peripherals(&mut result.peripherals, &result.constants);
                }
                "actions" => {
                    self.parse_actions(&mut result.actions);
                }
                "bootloader_exit" => {
                    self.parse_bootloader_exit(&mut result.init);
                }
                _ => {
                    self.parser.span_fatal(span, format!("unexpected block keyword '{}'", tok).as_str()).emit();
                    break;
                }
            }
        }

        result
    }

    /// Parses the `link_script => "some/path/script.ld";` syntax statement
    pub fn parse_link_script(&mut self, into: &mut common::McuInfo) {
        self.expect_ident_value("link_script");
        self.expect_fat_arrow();

        // read the literal and assert string
        let doc_src = self.parse_constant_literal();
        match doc_src {
            StaticValue::Str(v,_) => { into.link_script = v; }
            _ => { self.set_err_last("expected a string literal"); return; }
        }

        self.expect_semi();
    }

    /// Parses the entire `externs => { ... };` block.
    ///
    /// These externs will be used by the builder to generate the definitions so they can be used in the MCU code.
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

    /// Parses the entire `interrupts => [num_ints] @ .link_location { ... };` block.
    ///
    /// This block allows ranging of interrupts i.e. `1..5 => some_handler;` for easy bulk setting.
    /// However, if ranges overlap or an interrupt is set more than once a syntax error is placed.
    ///
    /// ast::Ident and ast::Path are the only supported types that can be assigned to an interrupt.
    ///
    /// When using a path as the handler, be sure to prefix it with `::` even though it will not be used in the
    /// generated code. This prefix is to simplify parsing between paths and idents and may go away in later versions.
    pub fn parse_interrupts(&mut self, into: &mut common::InterruptsInfo) {
        self.expect_ident_value("interrupts");
        self.expect_fat_arrow();

        // parse interrupt count
        self.expect_open_bracket();
        into.total_ints = match self.parse_uint::<u8>() {
            Ok(u) => {
                u.val as u8
            }
            Err(e) => {
				self.set_err(e.as_str());
                return;
            }
        };
        self.expect_close_bracket();

        // parse link section
        let exp_at = self.parser.expect(&token::Token::At);
        if exp_at.is_err() { exp_at.err().unwrap().emit(); }
        let exp_dot = self.parser.expect(&token::Token::Dot);
        if exp_dot.is_err() { exp_dot.err().unwrap().emit(); }
        into.link_location = format!(".{}", self.parse_ident_string());

        // make a bitmap to make sure we set an interrupt only once
        let mut set_ints: Bitmap<_, bitmap::DynamicSize> =
            Bitmap::from_storage(into.total_ints as usize, 1 as usize, vec![0; ((into.total_ints/8)+1) as usize])
                .expect("could not create bitmap");

        // read block
        self.expect_open_curly();
        while ! self.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            let span = self.parser.span.clone();
            let range = match self.parse_index_or_range() {
                Ok(v) => { v }
                Err(e) => {
                    self.parser.span_fatal(span, format!("could not parse inde/range: {}", e).as_str())
                        .emit();
                    return;
                }
            };
            self.expect_fat_arrow();
            for i in range.begin..range.end+1 {
                if set_ints.get(i).expect("error getting bitmap index") == 1 {
                    self.parser.span_fatal(span, "a value for this interrupt has already been set -- the ranges probably overlap")
                        .emit();
                }
                set_ints.set(i, 1);
            }

            let sp = self.parser.span.clone();
            let fn_ident = if token_is_path!(self) {
                    let res = self.parser.parse_path(PathStyle::Expr);
                    if res.is_err() { res.err().unwrap().emit(); return; }
                    StaticValue::Path(res.unwrap(), sp)
                } else {
                    let id = self.get_ident();
                    StaticValue::Ident(id.name.to_string(), id, sp)
                };

            into.ints.push((range, fn_ident));
            self.expect_semi();
        }
        self.expect_semi();
    }

    /// Parses the NVIC information block, informing us of the address and number of priority bits.
    pub fn parse_nvic(&mut self, into: &mut common::NvicInfo) {
        into.span = self.parser.span.clone();
        self.expect_ident_value("nvic");
        self.expect_fat_arrow();
        if token_is_path!(self) {
            let pat = self.parser.parse_path(PathStyle::Type);
            if pat.is_err() { pat.err().unwrap().emit(); }
            else { into.trait_path = Some(pat.unwrap()); }
        }
        self.expect_open_curly();

        self.expect_ident_value("addr");
        self.expect_fat_arrow();
        into.addr = match self.parse_uint::<u32>() {
            Ok(u) => {
                u.val as u32
            }
            Err(e) => {
				self.set_err(e.as_str());
                return;
            }
        };
        self.expect_semi();

        self.expect_ident_value("prio_bits");
        self.expect_fat_arrow();
        into.prio_bits = match self.parse_uint::<u8>() {
            Ok(u) => {
                u.val as u8
            }
            Err(e) => {
				self.set_err(e.as_str());
                return;
            }
        };
        self.expect_semi();

        self.expect_close_curly();
        self.expect_semi();
    }

    /// Parses the entire `memory => { ... };` block.
    pub fn parse_memory(&mut self, into: &mut common::McuInfo) {
        self.expect_ident_value("memory");
        self.expect_fat_arrow();
        self.expect_open_curly();

        while is_ident!(self.curr_token()) {
            let tok = extract_ident_name!(self);
            let span = self.parser.span;
            match tok.as_str() {
                "stack" => {
                    into.stack.span = self.parser.span.clone();
                    self.assert_keyword_preamble("stack");
                    self.parse_stack(&mut into.stack, &into.constants);
                }
                "data" => {
                    into.data.span = self.parser.span.clone();
                    self.assert_keyword_preamble("data");
                    self.parse_data(&mut into.data, &into.constants);
                }
                "heap" => {
                    into.heap.span = self.parser.span.clone();
                    self.assert_keyword_preamble("heap");
                    self.parse_heap(&mut into.heap, &into.constants);
                }
                "bss" => {
                    into.bss.span = self.parser.span.clone();
                    self.assert_keyword_preamble("bss");
                    self.parse_bss(&mut into.bss, &into.constants);
                }
                _ => {
                    self.parser.span_fatal(span, "unexpected keyword").emit();
                    return;
                }
            }
        }

        self.expect_close_curly();
        self.expect_semi();
    }


    /// Parses the entire `stack => { base => val @ .link_location; limit => val; };` block.
    ///
    /// Values to either the `base` or `limit` keywords can be either a numeric literal or identifier.
    ///
    /// The link location __must__ be a string, and prefixed with a period ('.').
    pub fn parse_stack(&mut self, into: &mut common::StackInfo, consts: &BTreeMap<String, StaticValue>) {
        // parse base
        self.expect_ident_value("base");
        self.expect_fat_arrow();
        into.base = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // parse limit
        self.expect_ident_value("limit");
        self.expect_fat_arrow();
        into.limit = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    /// Parses the entire `data => { src_begin => val; src_end => val; dest => val };` block.
    ///
    /// Values can be either a numeric literal or identifier.
    pub fn parse_data(&mut self, into: &mut common::DataInfo, consts: &BTreeMap<String, StaticValue>) {
        // parse src
        self.expect_ident_value("src_begin");
        self.expect_fat_arrow();
        into.src_begin = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // parse dest_begin
        self.expect_ident_value("src_end");
        self.expect_fat_arrow();
        into.src_end = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // parse dest_end
        self.expect_ident_value("dest");
        self.expect_fat_arrow();
        into.dest = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    /// Parses the entire `heap => { base => val; limit => val; };` block.
    ///
    /// Values can be either a numeric literal or identifier.
    pub fn parse_heap(&mut self, into: &mut common::HeapInfo, consts: &BTreeMap<String, StaticValue>) {
        // parse base
        self.expect_ident_value("base");
        self.expect_fat_arrow();
        into.base = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // parse limit
        self.expect_ident_value("limit");
        self.expect_fat_arrow();
        into.limit = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    /// Parses the entire `bss => { base => val; limit => val; };` block.
    ///
    /// Values can be either a numeric literal or identifier.
    pub fn parse_bss(&mut self, into: &mut common::BssInfo, consts: &BTreeMap<String, StaticValue>) {
        // parse base
        self.expect_ident_value("base");
        self.expect_fat_arrow();
        into.base = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // parse limit
        self.expect_ident_value("limit");
        self.expect_fat_arrow();
        into.limit = self.parse_lit_or_ident(consts);
        self.expect_semi();

        // TODO: how to validate values if they are idents!?

        self.expect_close_curly();
        self.expect_semi();
    }

    /// Parses the entire `peripherals => { name => ty_path @ ptr_loc; ... };` block.
    ///
    /// This is the block that associates all ioreg-defined peripherals with the generated MCU.
    /// Each peripheral is added as a field to the MCU structure and initialized with each ioreg's memory address.
    pub fn parse_peripherals(&mut self, into: &mut Vec<common::PeripheralInfo>, consts: &BTreeMap<String, StaticValue>) {
        while ! self.eat(&token::CloseDelim(token::DelimToken::Brace)) {
            let sp = self.parser.span;
            let name = self.parse_ident_string();
            self.expect_fat_arrow();
            let periph = self.parser.parse_ty_path().expect(format!("could not parse type path for peripheral {}", name).as_str());
            let exp_at = self.parser.expect(&token::Token::At);
            if exp_at.is_err() { exp_at.err().unwrap().emit(); }

            let addr = self.parse_lit_or_ident(consts);
            into.push(common::PeripheralInfo{name: name, path: periph, ptr: addr, span: sp});
            self.expect_semi();
        }
        self.expect_semi();
    }

    /// Parses the `actions => [ ... ];` block where the contents are valid Rust impl items.
    pub fn parse_actions(&mut self, into: &mut BTreeMap<String, common::ActionInfo>) {
        self.expect_ident_value("actions");
        self.expect_fat_arrow();
        self.expect_open_bracket();

        while ! self.eat(&token::CloseDelim(token::DelimToken::Bracket)) {
            let span = self.parser.span;

            let item_res = self.parser.parse_impl_item();
            if item_res.is_err() { item_res.err().unwrap().emit(); return ; }
            let item = item_res.unwrap();

            let name = item.ident.name.to_string();
            if into.contains_key(name.as_str()) {
                self.parser.span_fatal(span, "duplicate function definition here").emit(); break;
            }
            into.insert(name.clone(), common::ActionInfo{
                name: name,
                item: item,
                span: span,
            });
        }
        self.expect_semi();
    }

    /// Parses the init block that generates the `::init()` function.
    ///
    /// Consumes the entire `init => { watchdog => ident; exit => ident/path/uint; };` block.
    ///
    /// If the user does not define an `init(&self) {}` action, one is generated from the init block.
    pub fn parse_bootloader_exit(&mut self, into: &mut common::InitInfo) {
        into.span = self.parser.span;
        self.expect_ident_value("bootloader_exit");
        self.expect_fat_arrow();
        let sp = self.parser.span;
        into.exit = match &self.parser.token {
            &token::Token::Ident(_) => {
                if token_is_path!(self) {
                    let res = self.parser.parse_path(PathStyle::Expr);
                    if res.is_err() { res.err().unwrap().emit(); return; }
                    StaticValue::Path(res.unwrap(), sp)
                } else {
                    let id = self.get_ident();
                    StaticValue::Ident(id.name.to_string(), id, sp)
                }
            }
            &token::Token::ModSep => {
                let res = self.parser.parse_path(PathStyle::Expr);
                if res.is_err() { res.err().unwrap().emit(); return; }
                StaticValue::Path(res.unwrap(), sp)
            }
            &token::Token::Literal(_,_) => {
                self.parse_constant_literal()
            }
            _ => {
                self.parser.span_fatal(sp, "unexpected token type for exit function").emit();
                StaticValue::Uint(data::Unsigned::nospan(0).unwrap())
            }
        };
        self.expect_semi();
    }

    //
    // helpers
    //

    /// Helper to:
    ///     1. assert keyword
    ///     2. expect fat arrow
    ///     3. expect open curly
    pub fn assert_keyword_preamble(&mut self, expect: &str) {
        self.expect_ident_value(expect);
        self.expect_fat_arrow();
        self.expect_open_curly();
    }

    /// Parses the current token as either a literal or an ident.
    ///
    /// If the token is a literal, it is assumed to be an unsigned int. If not, a syntax error is placed.
    ///
    /// **NOTE:** will return a literal if the ident is an internal constant
    pub fn parse_lit_or_ident(&mut self, consts: &BTreeMap<String, StaticValue>) -> StaticValue {
        let sp = self.parser.span.clone();
        match self.curr_token() {
            &token::Token::Ident(id) => {
                if consts.contains_key(&id.name.to_string()) {
                    self.parser.bump(); // did not actually read anything
                    (*consts.get(&id.name.to_string()).expect("could not get constant from const map")).clone()
                } else {
                    StaticValue::Ident(id.name.to_string().clone(), self.get_ident(), sp)
                }
            }
            &token::Token::Literal(_, _) => {
                self.parse_constant_literal()
            }
            _ => {
                self.set_fatal_err("expected a literal or ident");
                StaticValue::Uint(data::Unsigned::nospan(0).unwrap())
            }
        }
    }

    /// Parses an expression in the form of either:
    ///     1. `5` i.e. a simple index
    ///     2. `3..7` i.e. a range
    ///
    /// **Note:** RangeInfo::width() is 0 for all indices (as opposed to a range).
    pub fn parse_index_or_range(&mut self) -> Result<common::RangeInfo, String> {
        let start = match self.parse_uint::<u8>() {
            Ok(v) => { v }
            Err(e) => { return Err(format!("failed to get start: {}", e)); }
        };
        let mut end = start;

        if self.parser.eat(&token::Token::DotDot) {
            end = match self.parse_uint::<u8>() {
                Ok(v) => { v }
                Err(e) => { return Err(format!("failed to get end: {}", e)); }
            };
            if end < start {
                self.set_fatal_err_last("range indices are inverted");
            }
        }

        Ok(common::RangeInfo{begin:start.val, end:end.val})
    }
}
