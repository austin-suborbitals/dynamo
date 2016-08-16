extern crate aster;
extern crate syntax;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::quote::rt::DUMMY_SP;

use std::collections::BTreeMap;

use parser::StaticValue;
use ::mcu::common;

#[derive(Debug)]
/// Structure representing either an index (single number) or a range (1..3).
///
/// **NOTE:** for a single index, end == begin.
pub struct RangeInfo {
    pub begin: usize,
    pub end: usize,
}

impl RangeInfo {
    /// Width of the range. If a single index, this is 0.
    pub fn width(&self) -> usize { self.end - self.begin }

    /// Boolean on whether `i` is equal to the index or in the range.
    pub fn contains(&self, i: usize) -> bool { i >= self.begin && i <= self.end }
}

#[derive(Debug)]
/// Internal structure representing the parsed `stack` block.
pub struct StackInfo {
    pub base:   StaticValue,
    pub limit:  StaticValue,
    pub ptr_link: String,
    pub span:   Span,
}

#[derive(Debug)]
/// Internal structure representing the parsed `data` block.
pub struct DataInfo {
    pub src_begin:  StaticValue,
    pub src_end:    StaticValue,
    pub dest:       StaticValue,
    pub span:       Span,
}

#[derive(Debug)]
/// Internal structure representing the parsed `heap` block.
pub struct HeapInfo {
    pub base:   StaticValue,
    pub limit:  StaticValue,
    pub span:   Span,
}

#[derive(Debug)]
/// Internal structure representing the parsed `interrupts` block.
pub struct InterruptsInfo {
    pub total_ints: u8,
    pub ints: Vec<(common::RangeInfo, StaticValue)>,
    pub link_location: String,
    pub span: Span,
}
impl InterruptsInfo {
    /// Generates a basic, nulled, interrupts structure that can be built into.
    pub fn default() -> Self {
        InterruptsInfo{
            total_ints: 0,
            ints: vec!(),
            link_location: "".to_string(),
            span: DUMMY_SP,
        }
    }
}

#[derive(Debug)]
/// Internal structure describing a peripheral to be used by the builder to add it to the MCU.
pub struct PeripheralInfo {
    pub name: String,
    pub path: ast::TyKind,
    pub ptr: StaticValue,
    pub span: Span,
}

#[derive(Debug)]
/// Internal structure for the builder which describes the parsed MCU block.
pub struct McuInfo {
    pub name: String,
    pub docs: Vec<String>,
    pub constants: BTreeMap<String, StaticValue>,
    pub externs: BTreeMap<String, (ast::TyKind, Span)>,
    pub interrupts: InterruptsInfo,
    pub stack: StackInfo,                                   // TODO: builder
    pub data: DataInfo,
    pub heap: HeapInfo,                                     // TODO: builder
    pub peripherals: Vec<PeripheralInfo>,
    pub entry_ptr_link: String,
    pub link_script: String,                                // TODO: builder and make sure #[link_flags = ""] escape crate-level
    pub span: Span,
    pub no_static: bool,
}

impl McuInfo {
    /// Returns a basic, nulled, MCU to be parsed into.
    pub fn default() -> Self {
        McuInfo{
            name: "".to_string(),
            docs: vec!(),
            constants: BTreeMap::new(),
            externs: BTreeMap::new(),
            interrupts: InterruptsInfo::default(),
            stack: StackInfo{
                base: StaticValue::default_uint(),
                limit: StaticValue::default_uint(),
                ptr_link: "".to_string(),
                span: DUMMY_SP,
            },
            data: DataInfo{
                src_begin:StaticValue::default_uint(),
                src_end:StaticValue::default_uint(),
                dest:StaticValue::default_uint(),
                span: DUMMY_SP,
            },
            heap: HeapInfo{
                base:StaticValue::default_uint(),
                limit:StaticValue::default_uint(),
                span: DUMMY_SP,
            },
            peripherals: vec!(),
            entry_ptr_link: "".to_string(),
            link_script: "".to_string(),
            span: DUMMY_SP,
            no_static: false,
        }
    }
}


