#![feature(rustc_private)]
#![feature(plugin_registrar)]

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use rustc_plugin::Registry;

pub mod ioreg;
pub mod cpu;


//
// plugin registration
//

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("ioreg", ioreg::expand_ioreg);
}
