#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

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
pub struct IoRegValDef {
    pub name:       String,
    pub value:      usize,  // TODO: generic? convert in the func def?
}

#[derive(Debug)]
pub struct IoRegFuncDef {
    pub name:       String,
    pub values:     Vec<FunctionValueType>,
    pub ty:         FunctionType,
}

#[derive(Debug)]
pub struct IoRegOffsetInfo {
    pub width:          u8,     // TODO: enum?
    pub access_perms:   RegisterPermissions,
    pub const_vals:     Vec<IoRegValDef>,
    pub functions:      HashMap<String, IoRegFuncDef>,
}

#[derive(Debug)]
pub struct IoRegSegmentInfo {
    pub name:           String,
    pub address:        u32, // TODO: usize?
    pub reg_width:      RegisterWidth,
    pub access_perms:   RegisterPermissions,
    pub offsets:        Vec<IoRegOffsetInfo>,
}


#[derive(Debug)]
pub struct IoRegInfo {
    pub name:       String,
    pub regions:    HashMap<String, IoRegSegmentInfo>,
}
