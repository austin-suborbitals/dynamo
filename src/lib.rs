#![feature(const_fn)]
#![feature(rustc_private)]
#![feature(plugin_registrar)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;

use rustc_plugin::Registry;

#[macro_use]
pub mod common;
pub mod ioreg;
pub mod mcu;

//
// plugin registration
//

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("ioreg", ioreg::expand_ioreg);
    reg.register_macro("ioreg_debug", ioreg::expand_ioreg_debug);

    reg.register_macro("mcu", mcu::expand_mcu);
    reg.register_macro("mcu_debug", mcu::expand_mcu_debug);
}
