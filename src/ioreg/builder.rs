extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::print::pprust;

use std::collections::HashMap;

use ::ioreg::common;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;

pub struct Builder {
    verbose: bool,
    base_builder: aster::AstBuilder,
}

impl Builder {
    pub fn new(verbose: bool) -> Builder {
        Builder {
            verbose: verbose,
            base_builder: aster::AstBuilder::new(),
        }
    }

    // TODO: better name?
    // TODO: better return?
    pub fn consume(&mut self, ioreg: &common::IoRegInfo) -> Vec<ptr::P<ast::Item>> {
        if self.verbose { println!("\n\n=====   Generating: {}   =====\n", ioreg.name); }

        let mut items: Vec<ptr::P<ast::Item>> = vec!();

        // generate the base struct
        items.push(
            self.base_builder.item().pub_().struct_(ioreg.name.clone())
                .build()
        );

        // grab the root-level constants to start the impl block
        let mut impl_build = self.base_builder.item().impl_();
        impl_build = self.build_const_vals(&ioreg.const_vals, impl_build);

        // now build the segments
        for s in &ioreg.segments {
            impl_build = self.build_const_vals(&s.1.const_vals, impl_build);

            if s.1.can_read() {
                // build ::read_foo()
                impl_build = self.build_getter(&s.1, impl_build);
            }
        }

        // push the impl definition
        let impl_item = impl_build.ty().id(ioreg.name.clone());
        if self.verbose { println!("{}", pprust::item_to_string(&impl_item)); }
        items.push(impl_item);

        items
    }

    fn build_const_vals(&mut self, vals: &HashMap<String, common::StaticValue>, prev_build: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_build;
        // generate associated constants in the impl block
        for v in vals {
            let name = v.0.to_uppercase();
            match v.1 {
                &common::StaticValue::Int(i, _) => {
                    builder = builder.item(name).const_().expr().i32(i).ty().i32();
                }
                &common::StaticValue::Uint(u, _) => {
                    builder = builder.item(name).const_().expr().u32(u).ty().u32();
                }
                &common::StaticValue::Float(_, ref s, _) => {
                    builder = builder.item(name).const_().expr().f32(s).ty().f32();
                }
                &common::StaticValue::Str(ref s, _) => {
                    builder = builder.item(name).const_().expr()
                        .str(s.clone().as_str())
                        .ty().ref_().lifetime("'static").ty().path().id("str").build();
                }
                &common::StaticValue::Error(ref e) => {
                    // TODO: what should we do here?
                    panic!("encountered error while building const_val getter: {}", e);
                }
            }
        }

        builder
    }

    fn build_getter(&mut self, seg: &common::IoRegSegmentInfo, prev_builder: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_builder;
        let fn_bldr = builder.method(format!("read_{}", seg.name)).fn_decl();
        match seg.reg_width {
            common::RegisterWidth::R8 => {
                builder = fn_bldr.return_().u8().block()
                    .expr().block().unsafe_()
                        .expr().call().id("volatile_load")
                            .arg().build_expr_kind(ast::ExprKind::Cast(
                                self.base_builder.expr().lit().u32(seg.address),
                                self.base_builder.ty().build_ty_kind(
                                    ast::TyKind::Ptr(ast::MutTy{
                                        ty: self.base_builder.ty().u8(),
                                        mutbl: ast::Mutability::Mutable
                                    })
                                )
                            )).build();
            }
            common::RegisterWidth::R16 => {
                builder = fn_bldr.return_().u16().block()
                    .expr().block().unsafe_()
                        .expr().call().id("volatile_load")
                            .arg().build_expr_kind(ast::ExprKind::Cast(
                                self.base_builder.expr().lit().u32(seg.address),
                                self.base_builder.ty().build_ty_kind(
                                    ast::TyKind::Ptr(ast::MutTy{
                                        ty: self.base_builder.ty().u16(),
                                        mutbl: ast::Mutability::Mutable
                                    })
                                )
                            ))
                        .build();
            }
            common::RegisterWidth::R32 => {
                builder = fn_bldr.return_().u32().block()
                    .expr().block().unsafe_()
                        .expr().call().id("volatile_load")
                            .arg().build_expr_kind(ast::ExprKind::Cast(
                                self.base_builder.expr().lit().u32(seg.address),
                                self.base_builder.ty().build_ty_kind(
                                    ast::TyKind::Ptr(ast::MutTy{
                                        ty: self.base_builder.ty().u32(),
                                        mutbl: ast::Mutability::Mutable
                                    })
                                )
                            ))
                        .build();
            }
            common::RegisterWidth::R64 => {
                builder = fn_bldr.return_().u64().block()
                    .expr().block().unsafe_()
                        .expr().call().id("volatile_load")
                            .arg().build_expr_kind(ast::ExprKind::Cast(
                                self.base_builder.expr().lit().u32(seg.address),
                                self.base_builder.ty().build_ty_kind(
                                    ast::TyKind::Ptr(ast::MutTy{
                                        ty: self.base_builder.ty().u64(),
                                        mutbl: ast::Mutability::Mutable
                                    })
                                )
                            ))
                        .build();
            }
            common::RegisterWidth::Unknown => {
                panic!("encountered register of unknown size!"); // TODO: no panic
            }
        };

        builder
    }
}
