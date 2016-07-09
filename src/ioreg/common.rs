extern crate aster;

#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

use syntax::ast;
use syntax::ptr;
use syntax::parse::token;
use syntax::codemap::Span;

use std::collections::HashMap;


//
// narrow from u32 to u8 and u16
//
pub trait Narrow<T> {
    fn narrow(u: T) -> Self;
}
impl Narrow<u32> for u8 {
    fn narrow(u: u32) -> Self { (u & 0xFFFF) as u8 }
}
impl Narrow<u32> for u16 {
    fn narrow(u: u32) -> Self { (u & 0xFFFF) as u16 }
}
impl Narrow<u32> for u32 {
    fn narrow(u: u32) -> Self { u }
}


//
// primitive type to_type and to_lit trait
//

pub trait ToTypeOrLitOrArg<T> {
    fn to_type() -> ptr::P<ast::Ty>;
    fn to_lit(val: T) -> ptr::P<ast::Expr>;
    fn to_arg(val: T) -> ptr::P<ast::Expr>;
}

// TODO: do not require a new builder
impl ToTypeOrLitOrArg<u8> for u8 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u8() }
    fn to_lit(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
    fn to_arg(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
}
// TODO: do not require a new builder
impl ToTypeOrLitOrArg<u16> for u16 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u16() }
    fn to_lit(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
    fn to_arg(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
}
// TODO: do not require a new builder
impl ToTypeOrLitOrArg<u32> for u32 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u32() }
    fn to_lit(val: u32) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(val) }
    fn to_arg(val: u32) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u32(val) }
}


//
// function value type
//

pub enum FunctionValueType {
    Static(u32),   // TODO: generic? convert in the func def?
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

#[derive(PartialEq)]
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

#[derive(PartialEq)]
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
    Unknown,
}

impl RegisterWidth {
    pub fn is_entire_register(&self, len: u8) -> bool {
        match self {
            &RegisterWidth::R8 => { len == 8 }
            &RegisterWidth::R16 => { len == 16 }
            &RegisterWidth::R32 => { len == 32 }
            &RegisterWidth::Unknown => { false }    // TODO: should this BE an error, got just get caught?
        }
    }
}

impl Debug for RegisterWidth {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            RegisterWidth::R8 => { write!(f, " r8") }
            RegisterWidth::R16 => { write!(f, "r16") }
            RegisterWidth::R32 => { write!(f, "r32") }
            RegisterWidth::Unknown => { write!(f, "???") }
        }
    }
}

//
// register offset index info
//

#[derive(Debug)]
pub struct IoRegOffsetIndexInfo {
    pub offset: u8,
    pub width: u8,
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
    pub index:          IoRegOffsetIndexInfo,
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
