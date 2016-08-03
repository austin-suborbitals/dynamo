extern crate aster;

#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

use syntax::ast;
use syntax::ptr;
use syntax::parse::token;
use syntax::codemap::Span;

use std::collections::BTreeMap;


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

pub trait ToAstType<T> {
    fn to_type() -> ptr::P<ast::Ty>;
    fn to_lit(val: T) -> ptr::P<ast::Expr>;
    fn to_arg(val: T) -> ptr::P<ast::Expr>;
}

// TODO: do not require a new builder
impl ToAstType<u8> for u8 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u8() }
    fn to_lit(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
    fn to_arg(val: u8) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u8(val) }
}
// TODO: do not require a new builder
impl ToAstType<u16> for u16 {
    fn to_type() -> ptr::P<ast::Ty> { aster::AstBuilder::new().ty().u16() }
    fn to_lit(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
    fn to_arg(val: u16) -> ptr::P<ast::Expr> { aster::AstBuilder::new().expr().lit().u16(val) }
}
// TODO: do not require a new builder
impl ToAstType<u32> for u32 {
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
            &FunctionValueType::Static(ref i) => { write!(f, "0x{:X}", i) }
            &FunctionValueType::Reference(ref r) => { write!(f, "{}", r.to_uppercase()) }
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
    Int(i32, String, Span),
    Uint(u32, String, Span),
    Float(f32, token::InternedString, String, Span), // TODO: avoid carrying the interned string around
    Str(String, String, Span),
    Error(String, Span),
}

impl Debug for StaticValue {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &StaticValue::Int(i, ref n, _) => { write!(f, "({}:int) {}", n, i) }
            &StaticValue::Uint(i, ref n, _) => { write!(f, "({}:uint) {}", n, i) }
            &StaticValue::Float(i, _, ref n, _) => { write!(f, "({}:float) {}", n, i) }
            &StaticValue::Str(ref s, ref n, _) => { write!(f, "({}:str) {}", n, s) }
            &StaticValue::Error(ref e, _) => { write!(f, "(PARSER ERROR) {}", e) }
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

    pub fn as_u8(&self) -> u8 {
        match self {
            &RegisterWidth::R8 => { 8 }
            &RegisterWidth::R16 => { 16 }
            &RegisterWidth::R32 => { 32 }
            &RegisterWidth::Unknown => { 0xFF }
        }
    }

    pub fn to_type_string(&self) -> &'static str {
        match self {
            &RegisterWidth::R8 => { "u8" }
            &RegisterWidth::R16 => { "u16" }
            &RegisterWidth::R32 => { "u32" }
            &RegisterWidth::Unknown => { "unknown" }
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
    pub width:  u8,
    pub span:   Span,
}

impl IoRegOffsetIndexInfo {
    pub fn width_is_byte_aligned(&self) -> bool {
        self.width.is_power_of_two() && self.width >= 8
    }

    pub fn offset_is_byte_aligned(&self) -> bool {
           self.offset == 0
        || (self.offset >= 8 && self.offset.is_power_of_two())
    }

    pub fn is_fully_byte_aligned(&self) -> bool {
        self.offset_is_byte_aligned() && self.width_is_byte_aligned()
    }

    pub fn offset_in_bytes(&self) -> u32 {
        ((self.offset as u32) - ((self.offset % 8) as u32)) / 8
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
    pub index:          IoRegOffsetIndexInfo,
    pub functions:      BTreeMap<String, IoRegFuncDef>,
    pub span:           Span,
}

#[derive(Debug)]
pub struct IoRegSegmentInfo {
    pub name:           String,
    pub address:        u32, // TODO: usize?
    pub reg_width:      RegisterWidth,
    pub access_perms:   RegisterPermissions,
    pub const_vals:     BTreeMap<String, StaticValue>,
    pub offsets:        Vec<IoRegOffsetInfo>,
    pub span:           Span,
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
    pub doc_srcs:   Vec<String>,
    pub segments:   BTreeMap<String, IoRegSegmentInfo>,
    pub const_vals: BTreeMap<String, StaticValue>,
    pub span:       Span,
}


#[cfg(test)]
mod tests {
    mod offset_index {
        use syntax::ext::quote::rt::DUMMY_SP;

        #[test]
        fn width_pow_of_two_at_index_zero() {
            for i in vec![8u8, 16, 32] {
                let off = super::super::IoRegOffsetIndexInfo{
                    offset: 0,
                    width: i,
                    span: DUMMY_SP,
                };

                assert!(off.width_is_byte_aligned());
                assert!(off.offset_is_byte_aligned());
                assert!(off.is_fully_byte_aligned());
            }
        }

        #[test]
        fn width_pow_of_two_at_index_pow_two() {
            for i in vec![8u8, 16, 32] {
                for w in vec![8u8, 16] {
                    let off = super::super::IoRegOffsetIndexInfo{
                        offset: i,
                        width: w,
                        span: DUMMY_SP,
                    };

                    assert!(off.width_is_byte_aligned());
                    assert!(off.offset_is_byte_aligned());
                    assert!(off.is_fully_byte_aligned());
                }
            }
        }

        #[test]
        fn width_but_not_offset_is_aligned() {
            let off = super::super::IoRegOffsetIndexInfo{
                offset: 7,
                width: 8,
                span: DUMMY_SP,
            };
            assert_eq!(true, off.width_is_byte_aligned());
            assert_eq!(false, off.offset_is_byte_aligned());
            assert_eq!(false, off.is_fully_byte_aligned());
        }
    }
}
