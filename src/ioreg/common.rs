#[allow(unused_imports)]
use std::fmt::{Debug, Formatter, Result};

use std::collections::HashMap;

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
pub struct IoRegSegmentInfo {
    pub name:           String,
    pub address:        u32, // TODO: usize?
    pub reg_width:      RegisterWidth,
    pub access_perms:   RegisterPermissions,
}


#[derive(Debug)]
pub struct IoRegInfo {
    pub name:       String,
    pub regions:    HashMap<String, IoRegSegmentInfo>,
}

