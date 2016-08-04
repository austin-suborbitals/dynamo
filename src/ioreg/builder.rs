extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::codemap::Span;
use syntax::print::pprust;

use std::ops::BitAnd;
use std::collections::BTreeMap;

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

macro_rules! ptr_cast {
    ($lhs:expr, $rhs:expr, true) => { ptr_cast!($lhs, $rhs, ast::Mutability::Mutable) };
    ($lhs:expr, $rhs:expr, false) => { ptr_cast!($lhs, $rhs, ast::Mutability::Immutable) };

    ($lhs:expr, $rhs:expr, $mutbl:expr) => {
        aster::AstBuilder::new().expr().build_expr_kind(
            ast::ExprKind::Cast( $lhs, ptr_type!($rhs, $mutbl) )
        )
    };
}

fn make_setter_doc(func: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, off: &common::IoRegOffsetInfo) -> String {
    let mut mask_qual = "";
    let mask_str = "\n\n**NOTE**: all values are as defined _before_ any masking.";
    
    if func.ty == common::FunctionType::Setter {
        return format!(
            "/// Writes the value of the given argument to the `{}` register at relative address 0x{:X} and offset {} bits.{}",
            seg.name, seg.address, off.index.offset, mask_str
        )
    }
    
    let mut actual_vals: Vec<String> = vec!();
    for i in &func.values {
        match i {
            &common::FunctionValueType::Static(val) => {
                // if we will be using a static value, and the qualifier has not been set, set it.
                if mask_qual.len() == 0 {
                    mask_qual = mask_str;
                }
                actual_vals.push(format!("0x{:X}", val));
            }
            &common::FunctionValueType::Reference(ref name) => {
                let scoped_name = format!("{}_{}", seg.name, name);
                if seg.const_vals.contains_key(&scoped_name) {
                    actual_vals.push(scoped_name.to_uppercase());
                } else {
                    actual_vals.push(name.to_uppercase());
                }
            }
            &common::FunctionValueType::Argument(_) => { /* do nothing */ }
        }
    }
    
    match actual_vals.len() {
        1 => {
            format!(
                "/// Writes the value `{}` to the `{}` register at relative address 0x{:X} and offset {} bits.{}",
                actual_vals[0], seg.name, seg.address, off.index.offset, mask_qual
            )
        }
        _ => {
            format!(
                "/// Consecutively writes the values `{:?}` to the `{}` register at relative address 0x{:X} and offset {} bits.{}",
                actual_vals, seg.name, seg.address, off.index.offset, mask_qual
            )
        }
    }
}

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

        // generate the base "struct" pointer.
        let mut docced_item = self.base_builder.item().span(self.reg.span)
            .attr().doc(
                format!("/// Pointer wrapper that implements the {} operations", self.reg.name).as_str()
            );
        if self.reg.doc_srcs.len() > 0 {
            docced_item = docced_item.attr().doc("///");
            for i in &self.reg.doc_srcs {
                docced_item = docced_item.attr().doc(format!("/// source: {}", i).as_str());
            }
        }

        let type_def = docced_item
            .pub_().tuple_struct(self.reg.name.clone())
                .with_tys(vec![ ptr_type!(self.base_builder.ty().u8(), false) ]) // false = immutable
            .build();
        items.push(type_def);

        // grab the root-level constants to start the impl block
        let mut impl_build = self.base_builder.item().span(self.reg.span).impl_();
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
        let impl_item = impl_build.ty().span(self.reg.span).id(self.reg.name.clone());
        items.push(impl_item);

        if self.verbose {
            for i in &items { println!("{}\n", pprust::item_to_string(i)); }
        }

        items
    }

    fn build_const_vals(&self, vals: &BTreeMap<String, common::StaticValue>, prev_build: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_build;
        // generate associated constants in the impl block
        for v in vals {
            let name = v.0.to_uppercase();
            match v.1 {
                &common::StaticValue::Int(i, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().i32(i).ty().i32();
                }
                &common::StaticValue::Uint(u, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().u32(u).ty().u32();
                }
                &common::StaticValue::Float(_, ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().f32(s).ty().f32();
                }
                &common::StaticValue::Str(ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr()
                        .str(s.clone().as_str())
                        .ty().ref_().lifetime("'static").ty().path().id("str").build();
                }
                &common::StaticValue::Error(ref e, _) => {
                    // TODO: what should we do here?
                    panic!("encountered error while building const_val getter: {}", e);
                }
            }
        }

        builder
    }

    fn build_getter(&self, seg: &common::IoRegSegmentInfo, prev_builder: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_builder;
        let fn_bldr = builder
            .item(format!("read_{}", seg.name)).attr().doc(
                format!("/// Reads the contents (as `{}`) of the `{}` register at relative address 0x{:X}",
                    seg.reg_width.to_type_string(), seg.name, seg.address
            ).as_str())
            .pub_().method().span(seg.span).fn_decl().self_().ref_();
        match seg.reg_width {
            common::RegisterWidth::R8 => {
                builder = fn_bldr.return_().u8().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u8>(seg.address, seg.span));
            }
            common::RegisterWidth::R16 => {
                builder = fn_bldr.return_().u16().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u16>(seg.address, seg.span));
            }
            common::RegisterWidth::R32 => {
                builder = fn_bldr.return_().u32().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u32>(seg.address, seg.span));
            }
            common::RegisterWidth::Unknown => {
                panic!("encountered register of unknown size!"); // TODO: no panic
            }
        };

        builder
    }

    // TODO: assumes u32 address space
    fn build_read_register<T>(&self, addr: u32, sp: Span) -> ptr::P<ast::Expr>
        where T: common::ToAstType<T>
    {
        let self_offset = self.base_builder.expr().span(sp)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(addr as isize)
            .build();

        // volatile_load(self.0.offset(0x1234) as *T)
        self.base_builder.expr().span(sp).call().id("volatile_load")
            .with_arg(  ptr_cast!(self_offset, T::to_type(), false)  ) // immutable
            .build()
    }

    fn build_volatile_store_base<T>(&self, addr: u32, sp: Span) -> FnArgsBuilder
        where T: common::ToAstType<T>
    {
        let self_offset = self.base_builder.expr().span(sp)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(addr as isize)
            .build();

        // volatile_store(self.0.offset(0x1234) as *T, 0x1234)
        self.base_builder.stmt().expr().span(sp).call().id("volatile_store")
            .with_arg(ptr_cast!(self_offset, T::to_type(), true)) // mutable
    }

    fn build_volatile_store<T>(
        &self, addr: u32, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, v: &common::FunctionValueType
    )
        -> ast::Stmt
        where T: common::ToAstType<T> + common::Narrow<u32>
    {
        let base = self.build_volatile_store_base::<T>(addr, fn_def.span);
        match fn_def.ty {
            common::FunctionType::StaticSetter => {
                base.with_arg( T::to_lit(self.get_uint_const_val::<T>(v, seg))  ).build()
            }
            common::FunctionType::Setter => {
                match v {
                    &common::FunctionValueType::Argument(ref arg_name) => {
                        base.with_arg( self.base_builder.expr().span(fn_def.span).id(arg_name) ).build()
                    }
                    _ => { panic!("did not expect non-argument type for FunctionType::Setter"); }
                }
            }
            common::FunctionType::Getter => {
                panic!("did not expect getter function for generating volatile_store");
            }
        }
    }

    fn get_uint_const_val<T>(&self, v: &common::FunctionValueType, seg: &common::IoRegSegmentInfo) -> T
        where T: common::ToAstType<T> + common::Narrow<u32>
    {
        match v {
            &common::FunctionValueType::Static(val) => { T::narrow(val) }
            &common::FunctionValueType::Reference(ref name) => {
                match self.lookup_const_val(name, seg) {
                    Ok(v) => {
                        match v {
                            &common::StaticValue::Uint(u, _, _) => { T::narrow(u) }
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
            &common::FunctionValueType::Argument(_) => { panic!("arguments cannot be looked up as constants"); }
        }
    }


    // if writing to a partial register, add a statement that says `let fetched = volatile_load(addr as *const T)`
    // we will mask this value and then do a `volatile_store(addr as *mut T, masked | val)`
    //
    // TODO: this function is a little heavy duty to be done in a loop -- same responsibilities, but
    //       perhaps pass in the mask and address so their "expensive" calculations can be reduced
    fn optimize_write<T>(&self,
        fn_def: &common::IoRegFuncDef, fn_prev: FnInternalBlockBuilder,
        off: &common::IoRegOffsetIndexInfo, seg: &common::IoRegSegmentInfo
    )
        -> FnInternalBlockBuilder
        where T: common::ToAstType<T> + common::Narrow<u32> + BitAnd<T>
    {
        let write_address = seg.address + off.offset_in_bytes();
        let shift_offset = off.offset % 8;

        // make a mask for the value as well based on the width of the partial register.
        // if you give the value 0xFF to a setter of a 3bit partial, we only take the lowest 3 bits i.e.
        //      (0xFF & 0b0111) == 0x05
        let mut val_mask: u32 = 0;
        for i in 0..off.width { val_mask |= 1 << i; }

        // this is what we will use to mask our "canvas" out of the read register
        let mask: T = T::narrow( !(val_mask << off.offset) );
        let mask_id = "fetched";

        // add a statement that reads the current value and ANDs it with our mask
        // i.e.: let mask_id = volatile_load(addr as *const T) & mask;
        let mut fn_block = fn_prev.stmt().span(fn_def.span)
            .let_()
                .id(mask_id.clone())
                .expr().build_bit_and(
                    self.build_read_register::<T>(write_address, fn_def.span),
                    T::to_lit(mask)
                );


        match fn_def.ty {
            common::FunctionType::Setter => {
                let arg_id = self.base_builder.expr().span(fn_def.span).id("val");
                let write_val = match off.offset {
                    0 => { arg_id }
                    _ => {
                        self.base_builder.expr().span(fn_def.span).paren().build_shl(
                            self.base_builder.expr().span(fn_def.span).paren().build_bit_and(
                                arg_id, T::to_lit(T::narrow(val_mask))
                            ),
                            self.base_builder.expr().span(fn_def.span).lit().u8(shift_offset)
                        )
                    }
                };

                fn_block = fn_block.with_stmt(
                    self.build_volatile_store_base::<T>(write_address, fn_def.span)
                        .with_arg(
                            self.base_builder.expr().span(fn_def.span).build_bit_or(
                                self.base_builder.expr().span(fn_def.span).id(mask_id),
                                write_val
                            )
                        )
                        .build()
                );
            }
            common::FunctionType::StaticSetter => {
                for v in &fn_def.values {
                    // if we are not writing the entire width, we cannot reliable set a portion of the register with a
                    // single binop. so instead, we read the current value and mask out the region we will be writing.
                    // now we can write the entire register and simply OR the needed value into the mask
                    let write_val = match off.offset {
                        0 => {
                            T::to_lit(T::narrow( self.get_uint_const_val::<u32>(v, seg) & val_mask ))
                        }
                        _ => {
                            self.base_builder.expr().span(fn_def.span).paren().build_shl(
                                T::to_lit( T::narrow( self.get_uint_const_val::<u32>(v, seg) & val_mask )),
                                self.base_builder.expr().span(fn_def.span).lit().u8(shift_offset)
                            )
                        }
                    };

                    fn_block = fn_block.with_stmt(
                        self.build_volatile_store_base::<T>(write_address, fn_def.span)
                            .with_arg(
                                self.base_builder.expr().span(fn_def.span).build_bit_or(
                                    self.base_builder.expr().span(fn_def.span).id(mask_id),
                                    write_val
                                )
                            )
                            .build()
                    );
                }
            }
            common::FunctionType::Getter => { panic!("optimize_write did not expect a getter function"); }
        }

        fn_block
    }



    fn build_setter<T>(&self, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, off: &common::IoRegOffsetIndexInfo,  fn_sig: FnInternalBlockBuilder)
        -> ImplBuilder
        where T: common::ToAstType<T> + common::Narrow<u32>
    {
        let mut fn_block = fn_sig;

        // if we are setting the entire register, or byte aligned length and index, then write the whole thing blindly :)
        if off.is_fully_byte_aligned() {
            // TODO: how to handle PARTIAL writes to WriteOnly registers?
            if seg.access_perms == common::RegisterPermissions::WriteOnly {
                panic!("partial writes to WriteOnly register indices is currently not supported");
            }

            let write_address = seg.address + off.offset_in_bytes();
            match fn_def.ty {
                common::FunctionType::Setter => {
                    let v = common::FunctionValueType::Argument("val".to_string());
                    fn_block = fn_block.with_stmt(self.build_volatile_store::<T>(write_address, &fn_def, &seg, &v));
                }
                common::FunctionType::StaticSetter => {
                    for v in &fn_def.values {
                        match off.width {
                            8 => {
                                fn_block = fn_block.with_stmt(self.build_volatile_store::<u8>(write_address, &fn_def, &seg, &v));
                            }
                            16 => {
                                fn_block = fn_block.with_stmt(self.build_volatile_store::<u16>(write_address, &fn_def, &seg, &v));
                            }
                            32 => {
                                fn_block = fn_block.with_stmt(self.build_volatile_store::<u32>(write_address, &fn_def, &seg, &v));
                            }
                            _ => { panic!("found non-aligned width despite alignment check"); }
                        }
                    }
                }
                common::FunctionType::Getter => {
                    panic!("did not expect getter function for generating a setter");
                }
            }
        } else {
            match off.width {
                1...8 => { fn_block = self.optimize_write::<u8>(fn_def, fn_block, off, seg); }
                9...16 => { fn_block = self.optimize_write::<u16>(fn_def, fn_block, off, seg); }
                17...32 => { fn_block = self.optimize_write::<u32>(fn_def, fn_block, off, seg); }
                _ => { panic!("unknown size for write optimization"); }
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

            let mut fn_base = bldr.item(f.1.name.clone())
                .attr().doc(make_setter_doc(f.1, seg, off).as_str())
                .pub_().method().span(f.1.span).fn_decl()
                    .self_().ref_();

            if f.1.ty == common::FunctionType::Setter {
                //return self.build_user_input_setter();
                fn_base = fn_base.arg_id("val").with_ty(common::offset_width_to_ty(&off.index));
            }

            let ctx = fn_base
                .default_return().block()
                .expr().block().unsafe_();

            bldr = match seg.reg_width {
                common::RegisterWidth::R8 => { self.build_setter::<u8>(&f.1, seg, &off.index, ctx) }
                common::RegisterWidth::R16 => { self.build_setter::<u16>(&f.1, seg, &off.index, ctx) }
                common::RegisterWidth::R32 => { self.build_setter::<u32>(&f.1, seg, &off.index, ctx) }
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
