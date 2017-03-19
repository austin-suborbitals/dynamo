extern crate aster;

use std;
use std::fmt::{Display, Debug};
use std::error::Error;
use std::marker::PhantomData;
use std::ops::{Add, Sub, Mul, Div, Rem};
use std::ops::{Not, BitAnd, BitOr, BitXor, Shl, Shr};

use syntax::{ast, ptr};
use syntax::codemap::Span;
use syntax::ext::quote::rt::DUMMY_SP;



extern crate num_traits;
use self::num_traits::{Num, NumCast, PrimInt, Bounded, Saturating};
use self::num_traits::{CheckedAdd, CheckedSub, CheckedDiv, CheckedMul};
use self::num_traits::{Zero, One, ToPrimitive};

use common::parser::ToAstType;


//--------------------------------------------------------------------
// address
//--------------------------------------------------------------------

/// Representation of an address on the target machine.
///
/// Internally held as a `usize`.
#[derive(Clone)]
pub struct Address(pub usize);

impl Bounded for Address {
    fn min_value() -> Self { Address(usize::min_value()) }
    fn max_value() -> Self { Address(usize::max_value()) }
}

impl std::ops::Add<Address> for Address {
    type Output = Address;
    fn add(self, other: Address) -> Address {
        Address(self.0 + other.0)
    }
}
impl std::ops::Sub<Address> for Address {
    type Output = Address;
    fn sub(self, other: Address) -> Address {
        Address(self.0 + other.0)
    }
}

impl std::ops::Add<usize> for Address {
    type Output = Address;
    fn add(self, other: usize) -> Address {
        Address(self.0 + other)
    }
}
impl std::ops::Sub<usize> for Address {
    type Output = Address;
    fn sub(self, other: usize) -> Address {
        Address(self.0 + other)
    }
}


//--------------------------------------------------------------------
// register
//--------------------------------------------------------------------

#[derive(PartialEq)]
/// Represents the acces permissions of a register. Guards against writes/reads to registers that do not support it.
#[derive(Copy, Clone)]
pub enum RegisterPermissions {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    Unknown,
}

impl RegisterPermissions {
    /// Truthy value on whether the permissions allow reads
    pub fn can_read(&self) -> bool {
        match self {
            &RegisterPermissions::ReadOnly | &RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    /// Truthy value on whether the permissions allow writes
    pub fn can_write(&self) -> bool {
        match self {
            &RegisterPermissions::WriteOnly | &RegisterPermissions::ReadWrite  => { true }
            _ => { false }
        }
    }

    pub fn read_only(&self) -> bool { self == &RegisterPermissions::ReadOnly }
    pub fn read_write(&self) -> bool { self == &RegisterPermissions::ReadWrite }
    pub fn write_only(&self) -> bool { self == &RegisterPermissions::WriteOnly }
}

impl Debug for RegisterPermissions {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            RegisterPermissions::ReadOnly => { write!(f, "ro") }
            RegisterPermissions::WriteOnly => { write!(f, "wo") }
            RegisterPermissions::ReadWrite => { write!(f, "wr") }
            RegisterPermissions::Unknown => { write!(f, "??") }
        }
    }
}



/// Representation of a register which is just an `Address`, permissions, and an associated type
/// describing the width of the register by the datatype it will be written as.
pub struct Register<T> {
    pub addr: Address,
    pub perms: RegisterPermissions,

    ty: PhantomData<T>,
}

impl<T> Register<T> {
    pub fn new(addr: Address, perms: RegisterPermissions) -> Register<T> {
        Register{addr:addr, perms:perms, ty:PhantomData}
    }

    pub fn new_wo(addr: Address) -> Register<T> {
        Register::new(addr, RegisterPermissions::WriteOnly)
    }

    pub fn new_wr(addr: Address) -> Register<T> {
        Register::new(addr, RegisterPermissions::ReadWrite)
    }

    pub fn new_ro(addr: Address) -> Register<T> {
        Register::new(addr, RegisterPermissions::ReadOnly)
    }

    /// Width of the register in bytes.
    pub fn width(&self) -> usize { std::mem::size_of::<T>() }

    pub fn can_fit<U>(&self) -> bool { self.width() >= std::mem::size_of::<U>() }

    pub fn can_read(&self) -> bool { self.perms.can_read() }
    pub fn can_write(&self) -> bool { self.perms.can_write() }
}


/// Generic storage of Register<T>.
///
/// Inherently, this becomes a list of "supported" register sizes.
pub enum RegisterType {
    U8(Register<u8>),
    U16(Register<u16>),
    U32(Register<u32>),
    U64(Register<u64>),
}

macro_rules! reg_type_passthrough {
    ($name:ident, $ty:ty, $field:ident) => {
        pub fn $name(&self) -> $ty {
            match self {
                &RegisterType::U8(ref r) => {  r.$field.clone() }
                &RegisterType::U16(ref r) => { r.$field.clone() }
                &RegisterType::U32(ref r) => { r.$field.clone() }
                &RegisterType::U64(ref r) => { r.$field.clone() }
            }
        }
    };

    ($name:ident, $ty:ty, $field:ident, fn) => {
        pub fn $name(&self) -> $ty {
            match self {
                &RegisterType::U8(ref r) => {  r.$field() }
                &RegisterType::U16(ref r) => { r.$field() }
                &RegisterType::U32(ref r) => { r.$field() }
                &RegisterType::U64(ref r) => { r.$field() }
            }
        }
    };
}

impl RegisterType {
    pub fn new(num_bits: usize, addr: Address, perms: RegisterPermissions) -> Option<RegisterType> {
        match num_bits {
            1...8   => { Some(RegisterType::U8(Register::<u8>::new(addr, perms))) }
            9...16  => { Some(RegisterType::U16(Register::<u16>::new(addr, perms))) }
            17...32 => { Some(RegisterType::U32(Register::<u32>::new(addr, perms))) }
            33...64 => { Some(RegisterType::U64(Register::<u64>::new(addr, perms))) }
            _ => { None }
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            &RegisterType::U8(_) => {  "u8" }
            &RegisterType::U16(_) => { "u16" }
            &RegisterType::U32(_) => { "u32" }
            &RegisterType::U64(_) => { "u64" }
        }
    }

    pub fn to_type(&self, bldr: &aster::AstBuilder) -> ptr::P<ast::Ty> {
        match self {
            &RegisterType::U8(_) => { bldr.ty().u8() }
            &RegisterType::U16(_) => { bldr.ty().u16() }
            &RegisterType::U32(_) => { bldr.ty().u32() }
            &RegisterType::U64(_) => { bldr.ty().u64() }
        }
    }

    reg_type_passthrough!(addr, Address, addr);
    reg_type_passthrough!(perms, RegisterPermissions, perms);
    reg_type_passthrough!(width, usize, width, fn);
}

impl Debug for RegisterType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &RegisterType::U8(ref r)  => { write!(f, "u8 @ 0x{:X}", r.addr.0) }
            &RegisterType::U16(ref r) => { write!(f, "u16 @ 0x{:X}", r.addr.0) }
            &RegisterType::U32(ref r) => { write!(f, "u32 @ 0x{:X}", r.addr.0) }
            &RegisterType::U64(ref r) => { write!(f, "u64 @ 0x{:X}", r.addr.0) }
        }
    }
}



//--------------------------------------------------------------------
// partial register
//--------------------------------------------------------------------


pub struct RegisterSlice {
    pub width: usize,
    pub offset: usize,
    pub span: Span,
}
impl RegisterSlice {
    pub fn new(width: usize, offset: usize, sp: Span) -> RegisterSlice {
        RegisterSlice{width:width, offset:offset, span: sp}
    }

    fn check_byte_align(v: usize) -> bool {
           v == 0
        || (v >= 8 && (v % 8 == 0))
    }

    /// Test if the width of the slice is aligned to a byte boundry
    pub fn width_is_byte_aligned(&self) -> bool {
        Self::check_byte_align(self.width)
    }

    /// Test if the offset is aligned to a byte boundry, but in this case, can also be 0.
    pub fn offset_is_byte_aligned(&self) -> bool {
        Self::check_byte_align(self.offset)
    }

    /// Test if both the offset and the width are byte aligned.
    ///
    /// This is common as such:
    ///
    /// ```
    /// ----------------------
    /// |   high   |   low   |
    /// ----------------------
    /// ```
    ///
    /// ... where `high` and `low` are both `RegisterSlice` of this `PartialRegister`.
    pub fn is_fully_byte_aligned(&self) -> bool {
        self.offset_is_byte_aligned() && self.width_is_byte_aligned()
    }

    /// Checks if the width of the slice is able to be represented by a builtin uint _and_ has exact width.
    pub fn is_type_aligned(&self) -> bool {
        self.width.is_power_of_two() && ((self.width/8) <= std::mem::size_of::<usize>())
    }

    /// Checks if the width, offset, and type are all aligned.
    pub fn is_aligned(&self) -> bool {
        self.is_type_aligned() && self.is_fully_byte_aligned()
    }

    /// Number of full bytes into the register the offset indicates.
    pub fn offset_in_bytes(&self) -> usize {
        ((self.offset as usize) - ((self.offset % 8) as usize)) / 8
    }

    /// Convert the width of the register to the representative ast::Ty (i.e. r8 -> u8)
    pub fn width_ty(&self) -> ptr::P<ast::Ty> {
        match self.width {
            1...8 => { u8::to_type() }
            9...16 => { u16::to_type() }
            17...32 => { u32::to_type() }
            33...64 => { u32::to_type() }
            _ => { usize::to_type() } // this also catches 0 which should never happen
        }
    }
}
#[cfg(test)]
mod tests {
    mod register_slice {
        use super::super::RegisterSlice;
        use syntax::codemap::DUMMY_SP;

        #[test]
        fn width_pow_of_two_at_index_zero() {
            for i in vec![8u8, 16, 32] {
                let off = RegisterSlice::new(i as usize, 0, DUMMY_SP);

                assert!(off.width_is_byte_aligned());
                assert!(off.offset_is_byte_aligned());
                assert!(off.is_fully_byte_aligned());
            }
        }

        #[test]
        fn width_pow_of_two_at_index_pow_two() {
            for i in vec![8u8, 16, 32] {
                for w in vec![8u8, 16] {
                    let off = RegisterSlice::new(w as usize, i as usize, DUMMY_SP);

                    assert!(off.width_is_byte_aligned());
                    assert!(off.offset_is_byte_aligned());
                    assert!(off.is_fully_byte_aligned());
                }
            }
        }

        #[test]
        fn width_but_not_offset_is_aligned() {
            let off = RegisterSlice::new(8, 7, DUMMY_SP);

            assert_eq!(true, off.width_is_byte_aligned());
            assert_eq!(false, off.offset_is_byte_aligned());
            assert_eq!(false, off.is_fully_byte_aligned());
        }
    }
}



/// Representation of a partial `Register` such that the data is either not aligned or not a byte multiple.
///
/// This structure has the `PhantomData` of a `T` object that _can_ be used to represent this region.
/// For instance, a 1-7 bit region is a `u8` or a 17 bit region being a `u32`.
/// If the designer really hates you, imagine something like a 5 bit sequence spanning a byte-align-barrier
/// requiring a `u16`.
pub struct PartialRegister<T: PrimInt + Display> {
    pub addr: Address,
    pub slice: RegisterSlice,
    p: PhantomData<T>
}

impl<T: PrimInt + Display> PartialRegister<T> {
    pub fn new(addr: Address, w: usize, o: usize, sp: Span) -> PartialRegister<T> {
        PartialRegister::<T>{
            addr: addr,
            slice: RegisterSlice{width:w, offset:o, span: sp},
            p: PhantomData::<T>
        }
    }

    pub fn from(addr: Address, slice: RegisterSlice) -> PartialRegister<T> {
        PartialRegister::<T>{
            addr: addr,
            slice: slice,
            p: PhantomData::<T>
        }
    }

    pub fn format(&self, val: T) -> Result<T, String> {
        let used_bits = (std::mem::size_of::<T>() * 8) - (val.leading_zeros() as usize);
        if used_bits > self.slice.width {
            Err(format!("value needs {} bits, but partial only had {}", used_bits, self.slice.width))
        } else {
            Ok( (val << self.slice.offset) & self.mask() )
        }
    }

    /// Create a bitmask of type `T` where all bits in range `(offset, offset+width)` are set.
    pub fn mask(&self) -> T {
        let mut m = T::zero();
        for i in 0..self.slice.width { m =  m | T::from(1 << i).unwrap(); }

        m << (self.slice.offset as usize)
    }


    /// Convert the width of the register to the representative ast::Ty (i.e. r8 -> u8)
    pub fn width_ty(&self) -> ptr::P<ast::Ty> { self.slice.width_ty() }
}


/// Generic storage of PartialRegister<T>.
///
/// Inherently, this (also) becomes a list of "supported" register sizes.
pub enum PartialType {
    U8(PartialRegister<u8>),
    U16(PartialRegister<u16>),
    U32(PartialRegister<u32>),
    U64(PartialRegister<u64>),
}

macro_rules! part_reg_type_passthrough {
    ($name:ident, $ty:ty, $field:ident) => {
        pub fn $name(&self) -> $ty {
            match self {
                &PartialType::U8(ref r) => {  &r.$field }
                &PartialType::U16(ref r) => { &r.$field }
                &PartialType::U32(ref r) => { &r.$field }
                &PartialType::U64(ref r) => { &r.$field }
            }
        }
    };

    ($name:ident, $ty:ty, $field:ident, fn) => {
        pub fn $name(&self) -> $ty {
            match self {
                &PartialType::U8(ref r) => {  r.$field() }
                &PartialType::U16(ref r) => { r.$field() }
                &PartialType::U32(ref r) => { r.$field() }
                &PartialType::U64(ref r) => { r.$field() }
            }
        }
    };
}


impl PartialType {
    pub fn new(addr: Address, w: usize, o: usize, sp: Span) -> Option<PartialType> {
        match w {
            1...8   => { Some(PartialType::U8(PartialRegister::<u8>::new(addr, w, o, sp))) }
            9...16  => { Some(PartialType::U16(PartialRegister::<u16>::new(addr, w, o, sp))) }
            17...32 => { Some(PartialType::U32(PartialRegister::<u32>::new(addr, w, o, sp))) }
            33...64 => { Some(PartialType::U64(PartialRegister::<u64>::new(addr, w, o, sp))) }
            _ => { None }
        }
    }

    pub fn from(addr: Address, slice: RegisterSlice) -> Option<PartialType> {
        match slice.width {
            1...8   => { Some(PartialType::U8(PartialRegister::<u8>::from(addr, slice))) }
            9...16  => { Some(PartialType::U16(PartialRegister::<u16>::from(addr, slice))) }
            17...32 => { Some(PartialType::U32(PartialRegister::<u32>::from(addr, slice))) }
            33...64 => { Some(PartialType::U64(PartialRegister::<u64>::from(addr, slice))) }
            _ => { None }
        }
	}

    pub fn to_lit(&self, us: &Unsigned, bldr: &aster::AstBuilder) -> ptr::P<ast::Expr> {
        match self {
            &PartialType::U8(_) => {
                bldr.expr().span(us.span).lit().u8(us.val as u8)
            }
            &PartialType::U16(_) => {
                bldr.expr().span(us.span).lit().u16(us.val as u16)
            }
            &PartialType::U32(_) => {
                bldr.expr().span(us.span).lit().u32(us.val as u32)
            }
            &PartialType::U64(_) => {
                bldr.expr().span(us.span).lit().u64(us.val as u64)
            }
        }
    }

    pub fn mask(&self, bldr: &aster::AstBuilder) -> ptr::P<ast::Expr> {
        match self {
            &PartialType::U8(ref r) => {
                bldr.expr().lit().u8(r.mask())
            }
            &PartialType::U16(ref r) => {
                bldr.expr().lit().u16(r.mask())
            }
            &PartialType::U32(ref r) => {
                bldr.expr().lit().u32(r.mask())
            }
            &PartialType::U64(ref r) => {
                bldr.expr().lit().u64(r.mask())
            }
        }
    }

    pub fn not_mask(&self, bldr: &aster::AstBuilder) -> ptr::P<ast::Expr> {
        match self {
            &PartialType::U8(ref r) => {
                bldr.expr().lit().u8(!r.mask())
            }
            &PartialType::U16(ref r) => {
                bldr.expr().lit().u16(!r.mask())
            }
            &PartialType::U32(ref r) => {
                bldr.expr().lit().u32(!r.mask())
            }
            &PartialType::U64(ref r) => {
                bldr.expr().lit().u64(!r.mask())
            }
        }
    }

    part_reg_type_passthrough!(slice, &RegisterSlice, slice);
    part_reg_type_passthrough!(addr, &Address, addr);
    part_reg_type_passthrough!(width_ty, ptr::P<ast::Ty>, width_ty, fn);
}

impl Debug for PartialType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &PartialType::U8(ref r)  => { write!(f, "0x{:X}[{}:{}]", r.addr.0, r.slice.offset, r.slice.offset+r.slice.width) }
            &PartialType::U16(ref r) => { write!(f, "0x{:X}[{}:{}]", r.addr.0, r.slice.offset, r.slice.offset+r.slice.width) }
            &PartialType::U32(ref r) => { write!(f, "0x{:X}[{}:{}]", r.addr.0, r.slice.offset, r.slice.offset+r.slice.width) }
            &PartialType::U64(ref r) => { write!(f, "0x{:X}[{}:{}]", r.addr.0, r.slice.offset, r.slice.offset+r.slice.width) }
        }
    }
}


//--------------------------------------------------------------------
// unsigned integer type with an AST Span
//--------------------------------------------------------------------

/// Representation of a parsed numeric unsigned constant that is not an `Address`.
///
/// Internally held as a `usize`.
#[derive(Copy, Clone)]
pub struct Unsigned {
    pub val: usize,
    pub span: Span,
}

impl Unsigned {
    pub fn new(val: usize, sp: Span) -> Self { Unsigned{val:val, span:sp} }

    pub fn from<T: ToPrimitive>(val: T, sp: Span) -> Option<Self> {
        match val.to_usize() {
            Some(v) => { Some(Unsigned{val:v, span:sp}) }
            None => { None }
        }
    }

    pub fn nospan<T: ToPrimitive>(val: T) -> Option<Self> {
        match val.to_usize() {
            Some(v) => { Some(Unsigned{val:v, span:DUMMY_SP}) }
            None => { None }
        }
    }
}


// trait impls

impl Display for Unsigned {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "0x{:X}:(unsigned)", self.val)
    }
}
impl std::fmt::UpperHex for Unsigned {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{:X}", self.val)
    }
}

impl PartialEq for Unsigned {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}
impl Eq for Unsigned {}
impl Ord for Unsigned {
    fn cmp(&self, other: &Unsigned) -> std::cmp::Ordering {
        self.val.cmp(&other.val)
    }
}

impl PartialOrd for Unsigned {
    fn partial_cmp(&self, other: &Unsigned) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Num for Unsigned {
    type FromStrRadixErr = String;
    fn from_str_radix(s: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        match usize::from_str_radix(s, radix) {
            Ok(v) => { Ok(Unsigned{val:v, span:DUMMY_SP}) }
            Err(e) => { Err(format!("{}", e.description())) }
        }
    }
}
impl NumCast for Unsigned {
    fn from<T: ToPrimitive>(n: T) -> Option<Self> {
        match n.to_usize() {
            Some(v) => { Some(Unsigned{val:v, span:DUMMY_SP}) }
            None => { None }
        }
    }
}
impl Bounded for Unsigned {
    fn min_value() -> Self { Unsigned{val:usize::min_value(), span:DUMMY_SP} }
    fn max_value() -> Self { Unsigned{val:usize::max_value(), span:DUMMY_SP} }
}
impl PrimInt for Unsigned {
    fn count_ones(self) -> u32 { self.val.count_ones() }
    fn count_zeros(self) -> u32 { self.val.count_zeros() }
    fn leading_zeros(self) -> u32 { self.val.leading_zeros() }
    fn trailing_zeros(self) -> u32 { self.val.trailing_zeros() }
    fn rotate_left(self, n: u32) -> Self    { Unsigned{val:self.val.rotate_left(n),  span:self.span.clone()} }
    fn rotate_right(self, n: u32) -> Self   { Unsigned{val:self.val.rotate_right(n), span:self.span.clone()} }
    fn signed_shl(self, n: u32) -> Self     { Unsigned{val:self.val.signed_shl(n),   span:self.span.clone()} }
    fn signed_shr(self, n: u32) -> Self     { Unsigned{val:self.val.signed_shr(n),   span:self.span.clone()} }
    fn unsigned_shl(self, n: u32) -> Self   { Unsigned{val:self.val.unsigned_shl(n), span:self.span.clone()} }
    fn unsigned_shr(self, n: u32) -> Self   { Unsigned{val:self.val.unsigned_shr(n), span:self.span.clone()} }
    fn swap_bytes(self) -> Self             { Unsigned{val:self.val.swap_bytes(),    span:self.span.clone()} }
    fn from_be(x: Self) -> Self             { Unsigned{val:usize::from_be(x.val),    span:DUMMY_SP} }
    fn from_le(x: Self) -> Self             { Unsigned{val:usize::from_le(x.val),    span:DUMMY_SP} }
    fn to_be(self) -> Self                  { Unsigned{val:self.val.to_be(),         span:self.span.clone()} }
    fn to_le(self) -> Self                  { Unsigned{val:self.val.to_le(),         span:self.span.clone()} }
    fn pow(self, exp: u32) -> Self          { Unsigned{val:self.val.pow(exp),        span:self.span.clone()} }
}
impl Zero for Unsigned {
    fn zero() -> Self { Unsigned{val:0usize, span:DUMMY_SP} }
    fn is_zero(&self) -> bool { self.val == 0usize }
}
impl One for Unsigned {
    fn one() -> Self { Unsigned{val:1usize, span:DUMMY_SP} }
}


impl Saturating for Unsigned {
    fn saturating_add(self, v: Self) -> Self { Unsigned{val:self.val.saturating_add(v.val), span:self.span.clone()} }
    fn saturating_sub(self, v: Self) -> Self { Unsigned{val:self.val.saturating_sub(v.val), span:self.span.clone()} }
}

// checked math

impl CheckedAdd for Unsigned {
    fn checked_add(&self, v: &Self) -> Option<Self> {
        match self.val.checked_add(v.val) {
            Some(v) => { Some(Unsigned{val:v, span:self.span.clone()}) }
            None => { None }
        }
    }
}
impl CheckedSub for Unsigned {
    fn checked_sub(&self, v: &Self) -> Option<Self> {
        match self.val.checked_sub(v.val) {
            Some(v) => { Some(Unsigned{val:v, span:self.span.clone()}) }
            None => { None }
        }
    }
}
impl CheckedMul for Unsigned {
    fn checked_mul(&self, v: &Self) -> Option<Self> {
        match self.val.checked_mul(v.val) {
            Some(v) => { Some(Unsigned{val:v, span:self.span.clone()}) }
            None => { None }
        }
    }
}
impl CheckedDiv for Unsigned {
    fn checked_div(&self, v: &Self) -> Option<Self> {
        match self.val.checked_div(v.val) {
            Some(v) => { Some(Unsigned{val:v, span:self.span.clone()}) }
            None => { None }
        }
    }
}

// base math

impl Add for Unsigned {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output { Unsigned{val:self.val+rhs.val, span:self.span.clone()} }
}
impl Sub for Unsigned {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output { Unsigned{val:self.val-rhs.val, span:self.span.clone()} }
}
impl Mul for Unsigned {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output { Unsigned{val:self.val*rhs.val, span:self.span.clone()} }
}
impl Div for Unsigned {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output { Unsigned{val:self.val/rhs.val, span:self.span.clone()} }
}
impl Rem for Unsigned {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output { Unsigned{val:self.val%rhs.val, span:self.span.clone()} }
}
impl Not for Unsigned {
    type Output = Self;
    fn not(self) -> Self::Output { Unsigned{val:!self.val, span:self.span.clone()} }
}

// binops

impl BitOr for Unsigned {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output { Unsigned{val:self.val|rhs.val, span:self.span.clone()} }
}
impl BitAnd for Unsigned {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output { Unsigned{val:self.val&rhs.val, span:self.span.clone()} }
}
impl BitXor for Unsigned {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output { Unsigned{val:self.val^rhs.val, span:self.span.clone()} }
}
impl Shl<usize> for Unsigned {
    type Output = Self;
    fn shl(self, rhs: usize) -> Self::Output { Unsigned{val:self.val<<rhs, span:self.span.clone()} }
}
impl Shr<usize> for Unsigned {
    type Output = Self;
    fn shr(self, rhs: usize) -> Self::Output { Unsigned{val:self.val>>rhs, span:self.span.clone()} }
}


// conversion

impl ToPrimitive for Unsigned {
    fn to_i64(&self) -> Option<i64>     { Some(self.val as i64) }
    fn to_u64(&self) -> Option<u64>     { Some(self.val as u64) }

    fn to_isize(&self) -> Option<isize> { Some(self.val as isize) }
    fn to_i8(&self) -> Option<i8>       { Some(self.val as i8) }
    fn to_i16(&self) -> Option<i16>     { Some(self.val as i16) }
    fn to_i32(&self) -> Option<i32>     { Some(self.val as i32) }
    fn to_usize(&self) -> Option<usize> { Some(self.val) }
    fn to_u8(&self) -> Option<u8>       { Some(self.val as u8) }
    fn to_u16(&self) -> Option<u16>     { Some(self.val as u16) }
    fn to_u32(&self) -> Option<u32>     { Some(self.val as u32) }
    fn to_f32(&self) -> Option<f32>     { Some(self.val as f32) }
    fn to_f64(&self) -> Option<f64>     { Some(self.val as f64) }
}
