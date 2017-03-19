extern crate aster;

use std::fmt::{Debug, Formatter, Result};

use syntax::ast;
use syntax::ptr;
use syntax::codemap::Span;

use std::collections::BTreeMap;

use common::parser;
use common::data;


//
// function value type
//

/// Representation of register function types.
///
/// * Static meaning all values are explicitly literal.
/// * Argument meaning it takes arguments, and is used for setters only.
/// * Reference meaning it uses non-constant values and cannot be compile-time optimized with literals.
pub enum FunctionValueType {
    Static(data::Unsigned, Span),
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
impl FunctionType {
	pub fn is_write(self) -> bool {
		match self {
			FunctionType::Setter | FunctionType::StaticSetter => { true }
			_ => { false }
		}
	}

	pub fn has_arg(self) -> bool {
		match self {
			FunctionType::Setter => { true } 
			_ => { false }
		}
	}
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



#[derive(Debug)]
/// Internal descriptor for a function definition.
///
/// This includes the type of function and any values it may use.
pub struct FunctionDef {
    pub name:       String,
    pub values:     Vec<FunctionValueType>,
    pub ty:         FunctionType,
    pub span:       Span,
}



#[derive(Debug)]
/// Internal descriptor for an offset into a Segment.
///
/// Offsets are what have functions associated with them and represent a window into an "actual" register.
pub struct Partial {
    pub reg:            data::PartialType,
    pub functions:      BTreeMap<String, FunctionDef>,
}

impl Partial {
    pub fn addr(&self) -> usize { self.reg.addr().0 }
    pub fn offset(&self) -> usize { self.reg.slice().offset }
    pub fn width(&self) -> usize { self.reg.slice().width }
    pub fn slice(&self) -> &data::RegisterSlice { self.reg.slice() }
    pub fn to_lit(&self, us: &data::Unsigned, bldr: &aster::AstBuilder) -> ptr::P<ast::Expr> {
        self.reg.to_lit(us, bldr)
    }
}

#[derive(Debug)]
/// Internal description of a "segment" which groups one or more hardware registers.
///
/// Segments contain multiple `PartialRegister`s which themselves contain the functions
/// that act on that "register".
pub struct Segment {
    pub name:           String,
    pub reg:            data::RegisterType,
    pub const_vals:     BTreeMap<String, parser::StaticValue>,
    pub partials:       Vec<Partial>,
    pub span:           Span,
}
impl Segment {
    /// Truthy value on whether the segment can be read from.
    pub fn can_read(&self) -> bool {
        match self.reg.perms() {
            data::RegisterPermissions::ReadOnly | data::RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    /// Truthy value on whether the segment can be written to.
    pub fn can_write(&self) -> bool {
        match self.reg.perms() {
            data::RegisterPermissions::WriteOnly | data::RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    pub fn addr(&self) -> data::Address { self.reg.addr() }
}


#[derive(Debug)]
pub struct InitInfo {
    pub defined: bool,
    pub item: Option<ast::ImplItem>,
    pub span: Span,
}


#[derive(Debug)]
/// Internal representation used to build the ioreg definition.
///
/// Contains a logical grouping of mcu-proper-registers.
/// Also holds constants and documentation sources given in the macro expansion.
pub struct IOReg {
    pub name:       String,
    pub doc_srcs:   Vec<String>,
    pub segments:   BTreeMap<String, Segment>,
    pub const_vals: BTreeMap<String, parser::StaticValue>,
    pub init:       InitInfo,
    pub span:       Span,
}
