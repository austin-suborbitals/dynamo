extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::print::pprust;
use syntax::codemap::Spanned;

use std::collections::HashMap;

use ::ioreg::common;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;
type FnInternalBlockBuilder = aster::block::BlockBuilder<
    aster::expr::ExprBuilder<
        aster::block::BlockBuilder<
            aster::item::ItemImplMethodBuilder<
                aster::item::ItemImplBuilder<aster::invoke::Identity>
            >
        >
    >
>;
type FnArgsBuilder = aster::expr::ExprCallArgsBuilder<aster::stmt::StmtExprBuilder<aster::invoke::Identity>>;


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
                        .build_expr(self.build_read_register::<u8>(seg.address));
            }
            common::RegisterWidth::R16 => {
                builder = fn_bldr.return_().u16().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u16>(seg.address));
            }
            common::RegisterWidth::R32 => {
                builder = fn_bldr.return_().u32().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u32>(seg.address));
            }
            common::RegisterWidth::Unknown => {
                panic!("encountered register of unknown size!"); // TODO: no panic
            }
        };

        builder
    }

    fn build_impl_unsafe_fn_block(&self, bldr: ImplBuilder, fn_def: &common::IoRegFuncDef) -> FnInternalBlockBuilder {
        bldr.method(fn_def.name.clone()).fn_decl().span(fn_def.span)
            .default_return().block()
            .expr().block().unsafe_()
    }

    // TODO: assumes u32 address space
    fn build_read_register<T>(&self, addr: u32) -> ptr::P<ast::Expr>
        where T: common::ToTypeOrLitOrArg<T>
    {
        // volatile_load(addr as *u8)
        self.base_builder.expr().call().id("volatile_load")
            .arg().build_expr_kind(ast::ExprKind::Cast(
                self.base_builder.expr().lit().u32(addr),
                self.base_builder.ty().build_ty_kind(
                    ast::TyKind::Ptr(ast::MutTy{
                        ty: T::to_type(),
                        mutbl: ast::Mutability::Immutable
                    })
                )
            ))
            .build()
    }

    fn build_volatile_store_base<T>(&self, addr: u32) -> FnArgsBuilder
        where T: common::ToTypeOrLitOrArg<T>
    {
        self.base_builder.stmt().expr().call().id("volatile_store")
            .arg().build_expr_kind(ast::ExprKind::Cast(
                self.base_builder.expr().lit().u32(addr),    // TODO: assumes 32bit address
                self.base_builder.ty().build_ty_kind(
                    ast::TyKind::Ptr(ast::MutTy{
                        ty: T::to_type(),
                        mutbl: ast::Mutability::Mutable
                    })
                )
            ))
    }

    fn get_uint_const_val<T>(&self, v: &common::FunctionValueType, seg: &common::IoRegSegmentInfo) -> ptr::P<ast::Expr>
        where T: common::ToTypeOrLitOrArg<T> + common::Narrow<u32>
    {
        match v {
            &common::FunctionValueType::Static(val) => { T::to_lit(T::narrow(val)) }
            &common::FunctionValueType::Reference(ref name) => {
                match self.lookup_const_val(name, seg) {
                    Ok(v) => {
                        match v {
                            &common::StaticValue::Uint(u, _) => { T::to_lit(T::narrow(u)) }
                            _ => {
                                panic!("expected a static value after lookup up constant value definition");
                            }
                        }
                    }
                    Err(e) => {
                        panic!(e);
                    }
                }
            }
        }
    }

    fn optimize_setter<T>(&self, off: &common::IoRegOffsetIndexInfo, val: &common::FunctionValueType, mask_id: &str, seg: &common::IoRegSegmentInfo)
        -> Vec<Spanned<ast::StmtKind>>
        where T: common::ToTypeOrLitOrArg<T> + common::Narrow<u32>
    {
        let mut result: Vec<Spanned<ast::StmtKind>> = vec!();

        // if we are not writing the entire width, we cannot reliable set a portion of the register with a
        // single binop. so instead, we read the current value and mask out the region we will be writing.
        // now we can write the entire register and simply OR the needed value into the mask
        let write_val = match off.offset {
            0 => {
                self.get_uint_const_val::<T>(val, seg)
            }
            _ => {
                self.base_builder.expr().build_shl(
                    self.get_uint_const_val::<T>(val, seg),
                    self.base_builder.expr().lit().u8(off.offset)
                )
            }
        };

        let store_base = self.build_volatile_store_base::<T>(seg.address);

        result.push(
            store_base.with_arg(
                self.base_builder.expr().build_bit_or(
                    self.base_builder.expr().id(mask_id),
                    write_val
                )
            ).build()
        );

        result
    }

    fn build_setter<T>(&self, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, off: &common::IoRegOffsetIndexInfo,  prev: ImplBuilder)
        -> ImplBuilder
        where T: common::ToTypeOrLitOrArg<T> + common::Narrow<u32>
    {
        // make an unsafe block for this function
        let mut fn_block = self.build_impl_unsafe_fn_block(prev, fn_def).span(fn_def.span);

        // TODO: how to handle PARTIAL writes to WriteOnly registers?
        if ! off.is_fully_byte_aligned() && seg.access_perms == common::RegisterPermissions::WriteOnly {
            panic!("partial writes to WriteOnly register indices is currently not supported");
        }

        match off.is_fully_byte_aligned() {
            true => {
                // if we are setting the entire register, or byte aligned length and index, then write the whole thing blindly :)
                for v in &fn_def.values {
                    let (fn_base, fn_value) = match off.width {
                        8 => {
                            (self.build_volatile_store_base::<u8>(seg.address), self.get_uint_const_val::<u8>(v, seg))
                        }
                        16 => {
                            (self.build_volatile_store_base::<u16>(seg.address), self.get_uint_const_val::<u16>(v, seg))
                        }
                        32 => {
                            (self.build_volatile_store_base::<u32>(seg.address), self.get_uint_const_val::<u32>(v, seg))
                        }
                        _ => { panic!("cannot create volatile store function base for sizes > 32bit"); }
                    };
                    fn_block = fn_block.with_stmt(fn_base.with_arg(fn_value).build());
                }
            }
            false => {
                // if writing to a partial register, add a statement that says `let fetched = volatile_load(addr as *const T)`
                // we will mask this value and then do a `volatile_store(addr as *mut T, masked | val)`
                let mut mask: u32 = 0;
                for i in 0..off.width {
                    mask |= 1 << (off.offset + if i > 0 { (i-1) } else { 0 });
                }
                mask = !mask;

                // save a common name for the ident we use to store the current value
                let mask_id = "fetched";

                // add a statement that reads the current value and ANDs it with our mask
                // i.e.: let mask_id = volatile_load(addr as *const T) & mask;
                fn_block = fn_block.stmt()
                    .let_()
                        .id(mask_id.clone())
                        .expr().build_bit_and(
                            self.build_read_register::<T>(seg.address),
                            T::to_lit(T::narrow(mask))
                        );

                for v in &fn_def.values {
                    // if not, optimize the setter based on how many bits and where the bits are
                    for s in self.optimize_setter::<T>(off, v, mask_id, seg) {
                        fn_block = fn_block.with_stmt(s);
                    }
                }
            }
        }

        fn_block.build()
    }

    fn build_setters(&self, off: &common::IoRegOffsetInfo, seg: &common::IoRegSegmentInfo, prev: ImplBuilder)
        -> ImplBuilder {

        let mut bldr = prev;
        for f in &off.functions {
            // we know how many bits we need to operate on, but we can only address bytes...
            // so, if we have less than 8 bits, operate on a byte, otherwise get the pow_2 >= width

            bldr = match seg.reg_width {
                common::RegisterWidth::R8 => { self.build_setter::<u8>(&f.1, seg, &off.index, bldr) }
                common::RegisterWidth::R16 => { self.build_setter::<u16>(&f.1, seg, &off.index, bldr) }
                common::RegisterWidth::R32 => { self.build_setter::<u32>(&f.1, seg, &off.index, bldr) }
                _ => {
                    // TODO: proper error
                    panic!(format!("unknown register width to create setter function in segment '{}'", seg.name));
                }
            };
        }
        bldr
    }

    fn lookup_const_val<'a>(&'a self, name: &str, seg: &'a common::IoRegSegmentInfo) -> Result<&common::StaticValue, String> {
        // first, check for global def
        let mut val = self.reg.const_vals.get(name);
        if val.is_some() {
            return Ok(val.unwrap());
        }

        // now check the segment for the scoped name
        let scoped_name: String = format!("{}_{}", seg.name, name);
        val = seg.const_vals.get(scoped_name.as_str());
        if val.is_some() {
            return Ok(val.unwrap());
        };

        Err(format!("could not find definition for '{}' or '{}'", name.to_string(), scoped_name))
    }
}
