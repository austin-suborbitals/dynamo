#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

use syntax::parse::token;
use syntax::codemap::Span;

use std::collections::HashMap;

//
// function value type
//

pub enum FunctionValueType {
    Static(usize),   // TODO: generic? convert in the func def?
    Reference(String),
}

impl Debug for FunctionValueType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &FunctionValueType::Static(ref i) => { write!(f, "{}", i.to_string()) }
            &FunctionValueType::Reference(ref r) => { write!(f, "{}", r) }
        }
    }
}


//
// function type
//

pub enum FunctionType {
    Getter,
    Setter,
}

impl Debug for FunctionType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            FunctionType::Getter => { write!(f, "getter") }
            FunctionType::Setter => { write!(f, "setter") }
        }
    }
}

//
// static value type
//

// TODO: 32bit limitation imposed here
pub enum StaticValue {
    Int(i32, String),
    Uint(u32, String),
    Float(f32, token::InternedString, String), // TODO: avoid carrying the interned string around
    Str(String, String),
    Error(String),
}

impl Debug for StaticValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &StaticValue::Int(i, ref n) => { write!(f, "({}:int) {}", n, i) }
            &StaticValue::Uint(i, ref n) => { write!(f, "({}:uint) {}", n, i) }
            &StaticValue::Float(i, _, ref n) => { write!(f, "({}:float) {}", n, i) }
            &StaticValue::Str(ref s, ref n) => { write!(f, "({}:str) {}", n, s) }
            &StaticValue::Error(ref e) => { write!(f, "(PARSER ERROR) {}", e) }
        }
    }
}

//
// register access permissions
//

pub enum RegisterPermissions {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    Unknown,
}

impl Debug for RegisterPermissions {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            RegisterPermissions::ReadOnly => { write!(f, "ro") }
            RegisterPermissions::WriteOnly => { write!(f, "wo") }
            RegisterPermissions::ReadWrite => { write!(f, "wr") }
            RegisterPermissions::Unknown => { write!(f, "??") }
        }
    }
}

//
// register width
//

pub enum RegisterWidth {
    R8,
    R16,
    R32,
    R64,
    Unknown,
}

impl Debug for RegisterWidth {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            RegisterWidth::R8 => { write!(f, " r8") }
            RegisterWidth::R16 => { write!(f, "r16") }
            RegisterWidth::R32 => { write!(f, "r32") }
            RegisterWidth::R64 => { write!(f, "r64") }
            RegisterWidth::Unknown => { write!(f, "???") }
        }
    }
}


#[derive(Debug)]
pub struct IoRegFuncDef {
    pub name:       String,
    pub values:     Vec<FunctionValueType>,
    pub ty:         FunctionType,
    pub span:       Span,
}

#[derive(Debug)]
pub struct IoRegOffsetInfo {
    pub width:          u8, // TODO: enum?
    pub functions:      HashMap<String, IoRegFuncDef>,
}

#[derive(Debug)]
pub struct IoRegSegmentInfo {
    pub name:           String,
    pub address:        u32, // TODO: usize?
    pub reg_width:      RegisterWidth,
    pub access_perms:   RegisterPermissions,
    pub const_vals:     HashMap<String, StaticValue>,
    pub offsets:        Vec<IoRegOffsetInfo>,
}
impl IoRegSegmentInfo {
    pub fn push_offset(&mut self, off: IoRegOffsetInfo) {
        self.offsets.push(off);
    }

    pub fn can_read(&self) -> bool {
        match self.access_perms {
            RegisterPermissions::ReadOnly | RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    pub fn can_write(&self) -> bool {
        match self.access_perms {
            RegisterPermissions::WriteOnly | RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }
}


#[derive(Debug)]
pub struct IoRegInfo {
    pub name:       String,
    pub segments:   HashMap<String, IoRegSegmentInfo>,
    pub const_vals: HashMap<String, StaticValue>,
}
