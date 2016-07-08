extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::print::pprust;

use std::collections::HashMap;

use ::ioreg::common;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;

pub struct Builder {
    verbose: bool,
    reg: common::IoRegInfo,
    base_builder: aster::AstBuilder,
}

impl Builder {
    pub fn new(ioreg: common::IoRegInfo, verbose: bool) -> Builder {
        Builder {
            verbose: verbose,
            reg: ioreg,
            base_builder: aster::AstBuilder::new(),
        }
    }

    // TODO: better name?
    // TODO: better return?
    pub fn build(&self) -> Vec<ptr::P<ast::Item>> {
        if self.verbose { println!("\n\n=====   Generating: {}   =====\n", self.reg.name); }

        let mut items: Vec<ptr::P<ast::Item>> = vec!();

        // generate the base struct
        items.push(
            self.base_builder.item().pub_().struct_(self.reg.name.clone())
                .build()
        );

        // grab the root-level constants to start the impl block
        let mut impl_build = self.base_builder.item().impl_();
        impl_build = self.build_const_vals(&self.reg.const_vals, impl_build);

        // now build the segments
        for s in &self.reg.segments {
            impl_build = self.build_const_vals(&s.1.const_vals, impl_build);

            // TODO: support manual getter func decl
            if s.1.can_read() {
                // build ::read_foo()
                impl_build = self.build_getter(&s.1, impl_build);
            }

            if s.1.can_write() {
                for o in &s.1.offsets {
                    impl_build = self.build_setters(&o, &s.1, impl_build);
                }
            }
        }

        // push the impl definition
        let impl_item = impl_build.ty().id(self.reg.name.clone());
        if self.verbose { println!("{}", pprust::item_to_string(&impl_item)); }
        items.push(impl_item);

        items
    }

    fn build_const_vals(&self, vals: &HashMap<String, common::StaticValue>, prev_build: ImplBuilder) -> ImplBuilder {
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

    fn build_getter(&self, seg: &common::IoRegSegmentInfo, prev_builder: ImplBuilder) -> ImplBuilder {
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

    fn build_write_u8(&self, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, offset: u8, width: u8,  prev: ImplBuilder)
        -> ImplBuilder {

        let bldr = prev;

        // build a mask to null the read value so we can OR our value into it
        let mut mask = 0u8;
        for i in 0..width {
            mask |= 1 << (offset + if i > 0 { (i-1) } else { 0 });
        }
        mask = !mask;


        // make a base func decl
        let mut fn_decl = bldr.method(fn_def.name.clone()).fn_decl().span(fn_def.span)
                .default_return().block()
                .expr().block().unsafe_()
                    .stmt().let_id("masked_val")                                    // let masked_val = 
                    .bit_and()                                                      // (we will AND the following)
                        .call().id("volatile_load")                                 // volatile_load(addr as *u8)
                            .arg().build_expr_kind(ast::ExprKind::Cast(
                                self.base_builder.expr().lit().u32(seg.address),
                                self.base_builder.ty().build_ty_kind(
                                    ast::TyKind::Ptr(ast::MutTy{
                                        ty: self.base_builder.ty().u8(),
                                        mutbl: ast::Mutability::Immutable
                                    })
                                )
                            ))
                            .build()
                        .lit().u8(mask);                                            // our mask literal (this is the rhs of AND)

        for v in &fn_def.values {
            let fn_arg: u8 = match v {
                &common::FunctionValueType::Static(val) => {
                    val as u8
                }
                &common::FunctionValueType::Reference(ref name) => {
                    let actual_name = format!("{}_{}", seg.name, name);
                    let mut val = self.reg.const_vals.get(actual_name.as_str());    // check for global def
                    if val.is_none() {
                        val = seg.const_vals.get(actual_name.as_str());             // check for local def
                    }
                    if val.is_none() {
                        panic!(format!("no definition of '{}' in scope", name))     // TODO: keep calm
                    };

                    match val.unwrap() {
                        &common::StaticValue::Uint(u, _) => {
                            u as u8
                        }
                        _ => {
                            panic!("cannot write to register with non-uint value"); // TODO: keep calm
                        }
                    }
                }
            };

            fn_decl = fn_decl.stmt().expr().call().id("volatile_store")                    // volatile_load(addr as *u8)
                .arg().build_expr_kind(ast::ExprKind::Cast(
                    self.base_builder.expr().lit().u32(seg.address),
                    self.base_builder.ty().build_ty_kind(
                        ast::TyKind::Ptr(ast::MutTy{
                            ty: self.base_builder.ty().u8(),
                            mutbl: ast::Mutability::Mutable
                        })
                    )
                ))
                .arg().bit_or()
                    .id("masked_val")
                    .lit().u8(fn_arg)
                .build();
        }

        fn_decl.build()
    }

    fn build_setters(&self, off: &common::IoRegOffsetInfo, seg: &common::IoRegSegmentInfo, prev: ImplBuilder)
        -> ImplBuilder {

        let mut bldr = prev;
        for f in &off.functions {
            // we know how many bits we need to operate on, but we can only address bytes...
            // so, if we have less than 8 bits, operate on a byte, otherwise get the pow_2 >= width
            let op_width = if off.index.width < 8 { 8 } else { off.index.width.next_power_of_two() };

            bldr = match op_width {
                8 => {
                    self.build_write_u8(&f.1, seg, off.index.offset, off.index.width, bldr)
                }
                /*
                16 => {
                }
                32 => {
                }
                64 => {
                }
                */
                _ => {
                    // TODO: proper error
                    panic!("register operation width ({})is either too large or non power of 2", op_width);
                }
            };
        }
        bldr
    }
}
