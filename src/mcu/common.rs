extern crate aster;
extern crate syntax;

use syntax::ast;
use syntax::codemap::Span;

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
    pub link_location: String,
}

#[derive(Debug)]
pub struct DataInfo {
    pub src:        StaticValue,
    pub dest_begin: StaticValue,
    pub dest_end:   StaticValue,
}

#[derive(Debug)]
pub struct HeapInfo {
    pub base:   StaticValue,
    pub limit:  StaticValue,
}

#[derive(Debug)]
pub struct InterruptsInfo {
    pub ints: Vec<(common::RangeInfo, StaticValue)>,
    pub link_location: String,
}
impl InterruptsInfo {
    pub fn default() -> Self {
        InterruptsInfo{
            ints: vec!(),
            link_location: "unknown_link_location".to_string(),
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
}

impl McuInfo {
    pub fn default() -> Self {
        McuInfo{
            name: "".to_string(),
            docs: vec!(),
            constants: BTreeMap::new(),
            externs: BTreeMap::new(),
            interrupts: InterruptsInfo::default(),
            stack: StackInfo{base: StaticValue::default_uint(), limit: StaticValue::default_uint(), link_location:"".to_string()},
            data: DataInfo{
                src:StaticValue::default_uint(),
                dest_begin:StaticValue::default_uint(),
                dest_end:StaticValue::default_uint()
            },
            heap: HeapInfo{base:StaticValue::default_uint(), limit:StaticValue::default_uint()},
            peripherals: vec!(),
            link_script: "".to_string(),
        }
    }
}


