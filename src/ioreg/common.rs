extern crate aster;

#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

use syntax::ast;
use syntax::ptr;
use syntax::codemap::Span;

use std::collections::BTreeMap;

use ::parser;
use parser::ToAstType;


//
// primitive type to_type and to_lit trait
//

/// Convert the width of the register to the representative ast::Ty (i.e. R8 -> u8)
pub fn reg_width_to_ty(width: &RegisterWidth) -> ptr::P<ast::Ty> {
    match width {
        &RegisterWidth::R8 => { u8::to_type() }
        &RegisterWidth::R16 => { u16::to_type() }
        &RegisterWidth::R32 => { u32::to_type() }
        &RegisterWidth::Unknown => { u32::to_type() } // TODO: panic? ok to default?
    }
}


//
// function value type
//

/// Representation of register function types.
///
/// * Static meaning all values are explicitly literal.
/// * Argument meaning it takes arguments, and is used for setters only.
/// * Reference meaning it uses non-constant values and cannot be compile-time optimized with literals.
pub enum FunctionValueType {
    Static(u32, Span),   // TODO: generic? convert in the func def?
    Argument(String, Span),
    Reference(String, Span),
}

impl Debug for FunctionValueType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &FunctionValueType::Static(ref i, _) => { write!(f, "0x{:X}", i) }
            &FunctionValueType::Argument(ref i, _) => { write!(f, "{}", i) }
            &FunctionValueType::Reference(ref r, _) => { write!(f, "{}", r.to_uppercase()) }
        }
    }
}


//
// function type
//

#[derive(Copy)]
#[derive(Clone)]
#[derive(PartialEq)]
/// Internal representation of function-type classification.
///
/// * A Getter is a reader function.
/// * A Setter is a setter function that either takes arguments, or uses FunctionValueType::Reference values
/// * A StaticSetter is also a setter, but uses only compile-time constants or literals.
pub enum FunctionType {
    Getter,
    Setter,
    StaticSetter,
}

impl Debug for FunctionType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            FunctionType::Getter => { write!(f, "getter") }
            FunctionType::Setter => { write!(f, "setter (with input)") }
            FunctionType::StaticSetter => { write!(f, "setter") }
        }
    }
}

//
// register access permissions
//

#[derive(PartialEq)]
/// Represents the acces permissions of a register. Guards against writes/reads to registers that do not support it.
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

/// Represents the width of the register. Curretnly, only supports registers up to 32bits.
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
/// Represents the offset from the segment, and the index into the register of a register-portion.
///
/// This is used to define where functions can access and gives mask/optimization info to the Builder.
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

/// Converts the offset+index information into the smallest primitive numeric type we can.
///
/// For example, for a 4bit value we use a u8, but for a 17bit value we must use a u32.
pub fn offset_width_to_ty(off: &IoRegOffsetIndexInfo) -> ptr::P<ast::Ty> {
    if off.width == 0 {
        panic!("cannot support offset widths of 0"); // TODO: check before calling?
    }

    if off.width < 16 {
        return u8::to_type();
    } else if off.width >= 16 && off.width < 32 {
        return u16::to_type();
    } else {
        return u32::to_type();
    }
}


#[derive(Debug)]
/// Internal descriptor for a function definition.
///
/// This includes the type of function and any values it may use.
pub struct IoRegFuncDef {
    pub name:       String,
    pub values:     Vec<FunctionValueType>,
    pub ty:         FunctionType,
    pub span:       Span,
}

#[derive(Debug)]
/// Internal descriptor for an offset into a Segment.
///
/// Offsets are what have functions associated with them and represent a window into an "actual" register.
pub struct IoRegOffsetInfo {
    pub index:          IoRegOffsetIndexInfo,
    pub functions:      BTreeMap<String, IoRegFuncDef>,
    pub span:           Span,
}

#[derive(Debug)]
/// Internal representation of a Segment.
///
/// Segments are typically what the mcu will expose as a register, but we want to group all related segments into one struct.
/// Segments contain multiple offsets which themselves contain the functions that act on that "register".
pub struct IoRegSegmentInfo {
    pub name:           String,
    pub address:        u32, // TODO: usize?
    pub reg_width:      RegisterWidth,
    pub access_perms:   RegisterPermissions,
    pub const_vals:     BTreeMap<String, parser::StaticValue>,
    pub offsets:        Vec<IoRegOffsetInfo>,
    pub span:           Span,
}
impl IoRegSegmentInfo {
    /// Add an offset to the segment.
    pub fn push_offset(&mut self, off: IoRegOffsetInfo) {
        self.offsets.push(off);
    }

    /// Truthy calue on whether the segment can be read from.
    pub fn can_read(&self) -> bool {
        match self.access_perms {
            RegisterPermissions::ReadOnly | RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    /// Truthy calue on whether the segment can be written to.
    pub fn can_write(&self) -> bool {
        match self.access_perms {
            RegisterPermissions::WriteOnly | RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }
}


#[derive(Debug)]
/// Internal representation used to build the ioreg definition.
///
/// Contains a logical grouping of mcu-proper-registers.
/// Also holds constants and documentation sources given in the macro expansion.
pub struct IoRegInfo {
    pub name:       String,
    pub doc_srcs:   Vec<String>,
    pub segments:   BTreeMap<String, IoRegSegmentInfo>,
    pub const_vals: BTreeMap<String, parser::StaticValue>,
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
