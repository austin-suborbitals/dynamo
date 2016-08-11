extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::print::pprust;

use ::mcu::common;
use ::mcu::parser;
use ::parser::StaticValue;


// TODO: dedupe with ioreg
macro_rules! ptr_type {
    ($ty:expr, true) => { ptr_type!($ty, ast::Mutability::Mutable) };
    ($ty:expr, false) => { ptr_type!($ty, ast::Mutability::Immutable) };

    ($ty:expr, $mutbl:expr) => {
        aster::AstBuilder::new().ty().build_ty_kind(
            ast::TyKind::Ptr(ast::MutTy{
                ty: $ty,
                mutbl: $mutbl
            })
        )
    };
}

// TODO: dedupe with ioreg
macro_rules! ptr_cast {
    ($lhs:expr, $rhs:expr, true) => { ptr_cast!($lhs, $rhs, ast::Mutability::Mutable) };
    ($lhs:expr, $rhs:expr, false) => { ptr_cast!($lhs, $rhs, ast::Mutability::Immutable) };

    ($lhs:expr, $rhs:expr, $mutbl:expr) => {
        aster::AstBuilder::new().expr().build_expr_kind(
            ast::ExprKind::Cast( $lhs, ptr_type!($rhs, $mutbl) )
        )
    };
}




pub struct Builder<'a> {
    verbose: bool,
    mcu: common::McuInfo,
    base_builder: aster::AstBuilder,
    parser: parser::Parser<'a>,
}

impl<'a> Builder<'a> {
    pub fn new(mcu: common::McuInfo, parser: parser::Parser<'a>, verbose: bool) -> Builder {
        Builder {
            verbose: verbose,
            mcu: mcu,
            base_builder: aster::AstBuilder::new(),
            parser: parser,
        }
    }

    // TODO: better name?
    // TODO: better return?
    pub fn build(&self) -> Vec<ptr::P<ast::Item>> {
        let mut result = Vec::<ptr::P<ast::Item>>::new();
        result.push(self.build_struct());
        result.push(self.build_static_instantiation());

        if self.verbose {
            for i in &result { println!("{}\n", pprust::item_to_string(i)); }
        }
        result
    }

    // TODO: lots of clones
    pub fn build_struct(&self) -> ptr::P<ast::Item> {
        // make the struct builder and add the doc attributes
        let mut preamble = self.base_builder.item()
            .attr().doc(format!("/// Generated definition of the {} MCU", self.mcu.name).as_str());
        for d in &self.mcu.docs {
            preamble = preamble.attr().doc(format!("/// source: {}", d).as_str());
        }
        if ! self.mcu.link_script.is_empty() {
            preamble = preamble.attr().doc(format!("/// link script: {}", self.mcu.link_script).as_str());
        }

        // make it a pub struct
        let mut base_struct = preamble.pub_().struct_(self.mcu.name.clone()).field("initialized").ty().bool();

        // and add the peripheral fields
        for p in &self.mcu.peripherals {
            base_struct = base_struct.field(p.name.clone()).pub_().ty().build_ty_kind(p.path.clone());
        }

        base_struct.build()
    }

    // TODO: minimize clones
    pub fn build_static_instantiation(&self) -> ptr::P<ast::Item> {
        //self.base_builder.stmt().let_().id("mcu").ty().id(self.mcu.name.clone())

        // TODO: do this in a .map(|x| x) style
        let mut built_struct = self.base_builder.expr().struct_().id(self.mcu.name.clone()).build()
            .field("initialized").false_();
        for p in &self.mcu.peripherals {
            let ty_path = match &p.path {
                &ast::TyKind::Path(_, ref path) => { path.clone() }
                _ => {
                    // TODO: set parser error
                    panic!(format!("cannot use non-path type for peripheral {}", p.name));
                }
            };

            match &p.ptr {
                &StaticValue::Uint(addr, _, sp) => {
                    built_struct = built_struct.span(sp).field(p.name.clone())
                        .call().build_path(ty_path).arg().build(
                            ptr_cast!(
                                self.base_builder.span(sp).expr().u32(addr),
                                self.base_builder.ty().u8(),
                                false
                            )
                        ).build();
                }
                &StaticValue::Ident(ref name, _, sp) => {
                    built_struct = built_struct.span(sp).field(p.name.clone())
                        .call().build_path(ty_path).arg().build(
                            ptr_cast!(
                                self.base_builder.span(sp).expr().id(name),
                                self.base_builder.ty().u8(),
                                false
                            )
                        ).build();
                }
                _ => {
                    // TODO: set parser error instead of panic
                    panic!(format!("must use either a literal or an ident for peripheral address for {}", p.name));
                }
            }
        }


        self.base_builder.item().build_item_kind(
            "mcu",
            ast::ItemKind::Static(
                self.base_builder.ty().id(self.mcu.name.clone()),
                ast::Mutability::Mutable,
                built_struct.build()
            )
        )
    }
}
