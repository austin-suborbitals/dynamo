extern crate aster;
extern crate syntax;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::quote::rt::DUMMY_SP;

use std::collections::BTreeMap;

use parser::StaticValue;
use ::mcu::common;

#[derive(Debug)]
pub struct RangeInfo {
    pub begin: usize,
    pub end: usize,
}

impl RangeInfo {
    pub fn width(&self) -> usize { self.end - self.begin }
    pub fn contains(&self, i: usize) -> bool { i >= self.begin && i <= self.end }
}

#[derive(Debug)]
pub struct StackInfo {
    pub base:   StaticValue,
    pub limit:  StaticValue,
    pub span:   Span,
}

#[derive(Debug)]
pub struct DataInfo {
    pub src_begin:  StaticValue,
    pub src_end:    StaticValue,
    pub dest:       StaticValue,
    pub span:       Span,
}

#[derive(Debug)]
pub struct HeapInfo {
    pub base:   StaticValue,
    pub limit:  StaticValue,
    pub span:   Span,
}

#[derive(Debug)]
pub struct InterruptsInfo {
    pub code_entry: usize,
    pub total_ints: u8,
    pub ints: Vec<(common::RangeInfo, StaticValue)>,
    pub link_location: String,
    pub span: Span,
}
impl InterruptsInfo {
    pub fn default() -> Self {
        InterruptsInfo{
            code_entry: (-1isize as usize),
            total_ints: 0,
            ints: vec!(),
            link_location: "".to_string(),
            span: DUMMY_SP,
        }
    }
}

#[derive(Debug)]
pub struct PeripheralInfo {
    pub name: String,
    pub path: ast::TyKind,
    pub ptr: StaticValue,
    pub span: Span,
}

#[derive(Debug)]
pub struct McuInfo {
    pub name: String,
    pub docs: Vec<String>,
    pub constants: BTreeMap<String, StaticValue>,
    pub externs: BTreeMap<String, (ast::TyKind, Span)>,
    pub interrupts: InterruptsInfo,                         // TODO: builder
    pub stack: StackInfo,                                   // TODO: builder
    pub data: DataInfo,                                     // TODO: builder
    pub heap: HeapInfo,                                     // TODO: builder
    pub peripherals: Vec<PeripheralInfo>,
    pub link_script: String,                                // TODO: builder and make sure #[link_flags = ""] escape crate-level
    pub span: Span,
    pub no_static: bool,
}

impl McuInfo {
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
            link_script: "".to_string(),
            span: DUMMY_SP,
            no_static: false,
        }
    }
}


