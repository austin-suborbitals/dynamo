extern crate syntax;

use syntax::ast;

use std::collections::BTreeMap;

use parser::StaticValue;

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
    pub base:   usize,
    pub limit:  usize,
}

#[derive(Debug)]
pub struct DataInfo {
    pub src:        usize,
    pub dest_begin: usize,
    pub dest_end:   usize,
}

#[derive(Debug)]
pub struct HeapInfo {
    pub base:   usize,
    pub limit:  usize,
}

#[derive(Debug)]
pub struct McuInfo {
    pub name: String,
    pub docs: Vec<String>,
    pub constants: BTreeMap<String, StaticValue>,
    pub interrupts: Vec<Option<fn()>>,
    pub stack: StackInfo,
    pub data: DataInfo,
    pub heap: HeapInfo,
    pub peripherals: Vec<ast::Path>
}

impl McuInfo {
    pub fn default() -> Self {
        McuInfo{
            name: "".to_string(),
            docs: vec!(),
            constants: BTreeMap::new(),
            interrupts: vec!(),
            stack: StackInfo{base:0, limit:0},
            data: DataInfo{src:0, dest_begin:0, dest_end:0},
            heap: HeapInfo{base:0, limit:0},
            peripherals: vec!(),
        }
    }
}


