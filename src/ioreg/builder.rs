extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::codemap::Span;
use syntax::print::pprust;
use syntax::ext::quote::rt::DUMMY_SP;

use std::fmt;
use std::ops::BitAnd;
use std::collections::BTreeMap;

use ::parser;
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


/// Creates a ptr type to the ast::Ty given as $ty with a bool for mutability.
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

/// Casts the $lhs argument as a pointer of type $rhs and takes a bool for mutability.
macro_rules! ptr_cast {
    ($lhs:expr, $rhs:expr, true) => { ptr_cast!($lhs, $rhs, ast::Mutability::Mutable) };
    ($lhs:expr, $rhs:expr, false) => { ptr_cast!($lhs, $rhs, ast::Mutability::Immutable) };

    ($lhs:expr, $rhs:expr, $mutbl:expr) => {
        aster::AstBuilder::new().expr().build_expr_kind(
            ast::ExprKind::Cast( $lhs, ptr_type!($rhs, $mutbl) )
        )
    };
}

/// Takes a StaticValue and expects a uint. If not, a syntax error is generated and dummy value of 0 returned.
macro_rules! expect_uint {
    ($self_:ident, $expect:ident, $t:ident) => {
        match $expect {
            &parser::StaticValue::Uint(u, _, _) => { $t::narrow(u) }

              &parser::StaticValue::Int(_,_,sp)
            | &parser::StaticValue::Float(_,_,_,sp)
            | &parser::StaticValue::Str(_,_,sp)
            | &parser::StaticValue::Ident(_,_,sp)
            | &parser::StaticValue::Path(_,sp)
            | &parser::StaticValue::Error(_,sp)
            => {
                $self_.parser.parser.span_fatal(sp,
                    "expected a static value after lookup up constant value definition").emit();
                $t::narrow(0u32)
            }
        }
    }
}

/// Used to have non-quoted strings in the Debug printer.
struct NonQuoteString(String);
impl fmt::Display for NonQuoteString {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> { write!(f, "{}", self.0) }
}
impl fmt::Debug for NonQuoteString {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> { write!(f, "{}", self.0) }
}

/// Generate documentation attributes for the given setter function.
fn make_setter_doc(func: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, off: &common::IoRegOffsetInfo) -> String {
    let mut mask_qual = "";
    let mask_str = "\n///\n/// **NOTE**: all values in unaligned writes are subject to masking which is not displayed here.";
    
    if func.ty == common::FunctionType::Setter {
        return format!(
            "/// Writes the value of the given argument to the `{}` register at relative address 0x{:X} and offset {} bits.{}",
            seg.name, seg.address, off.index.offset, if off.index.is_fully_byte_aligned() { "" } else { mask_str }
        )
    }
    
    let mut actual_vals: Vec<NonQuoteString> = vec!();
    for i in &func.values {
        if mask_qual.len() == 0 && ! off.index.is_fully_byte_aligned() {
            mask_qual = mask_str;
        }
        match i {
            &common::FunctionValueType::Static(val, _) => {
                // if we will be using a static value, and the qualifier has not been set, set it.
                actual_vals.push(NonQuoteString(format!("0x{:X}", val)));
            }
            &common::FunctionValueType::Reference(ref name, _) => {
                let scoped_name = format!("{}_{}", seg.name, name);
                if seg.const_vals.contains_key(&scoped_name) {
                    actual_vals.push(NonQuoteString(scoped_name.to_uppercase()));
                } else {
                    actual_vals.push(NonQuoteString(name.to_uppercase()));
                }
            }
            &common::FunctionValueType::Argument(_, _) => { /* do nothing */ }
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

/// Builds the AST from the parsed ioreg!() macro.
pub struct Builder<'a> {
    verbose: bool,
    reg: common::IoRegInfo,
    bldr: aster::AstBuilder,
    parser: ::ioreg::parser::Parser<'a>,
}

impl<'a> Builder<'a> {
    /// Consumes the parsed IoRegInfo as well as the parser (for later syntax error placement).
    pub fn new(ioreg: common::IoRegInfo, parser: ::ioreg::parser::Parser, verbose: bool) -> Builder {
        Builder {
            verbose: verbose,
            reg: ioreg,
            bldr: aster::AstBuilder::new(),
            parser: parser,
        }
    }

    // TODO: better name?
    // TODO: better return?
    /// Outputs Rust AST items representing the generated ioreg.
    pub fn build(&self) -> Vec<ptr::P<ast::Item>> {
        if self.verbose { println!("\n\n=====   Generating: {}   =====\n", self.reg.name); }

        let mut items: Vec<ptr::P<ast::Item>> = vec!();

        // generate the base "struct" pointer.
        let mut docced_item = self.bldr.item().span(self.reg.span)
            .attr().list("repr").word("C").build()
            .attr().list("derive").word("Debug").build()
            .attr().list("derive").word("Clone").build()
            .attr().list("derive").word("PartialEq").build()
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
                .field().pub_().build_ty( ptr_type!(self.bldr.ty().u8(), false) ) // false = immutable
            .build();
        items.push(type_def);

        // grab the root-level constants to start the impl block
        let mut impl_build = self.bldr.item().span(self.reg.span).impl_();
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

        // build and push the peripheral trait functions
        impl_build = self.build_peripheral_init_fns(impl_build);

        // push the impl definition
        let impl_item = impl_build.ty().span(self.reg.span).id(self.reg.name.clone());
        items.push(impl_item);

        if self.verbose {
            for i in &items { println!("{}\n", pprust::item_to_string(i)); }
        }

        items
    }

    /// Builds associated constants for the given ioreg as defined in the `constants => { ... }` block.
    fn build_const_vals(&self, vals: &BTreeMap<String, parser::StaticValue>, prev_build: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_build;
        // generate associated constants in the impl block
        for v in vals {
            let name = v.0.to_uppercase();
            match v.1 {
                &parser::StaticValue::Int(i, _, sp) => {
                    builder = builder.item(name).span(sp)
                        .attr().list("allow").word("dead_code").build()
                        .const_().expr().i32(i).ty().i32();
                }
                &parser::StaticValue::Uint(u, _, sp) => {
                    builder = builder.item(name).span(sp)
                        .attr().list("allow").word("dead_code").build()
                        .const_().expr().u32(u).ty().u32();
                }
                &parser::StaticValue::Float(_, ref s, _, sp) => {
                    builder = builder.item(name).span(sp)
                        .attr().list("allow").word("dead_code").build()
                        .const_().expr().f32(s).ty().f32();
                }
                &parser::StaticValue::Str(ref s, _, sp) => {
                    builder = builder.item(name).span(sp)
                        .attr().list("allow").word("dead_code").build()
                        .const_().expr().str(s.clone().as_str())
                        .ty().ref_().lifetime("'static").ty().path().id("str").build();
                }
                &parser::StaticValue::Ident(ref s, _, sp) => {
                    self.parser.parser.span_fatal(sp, format!("cannot use ident as constant: {}", s).as_str()).emit();
                }
                &parser::StaticValue::Path(ref s, sp) => {
                    self.parser.parser.span_fatal(sp, format!("cannot use path as constant: {}", s).as_str()).emit();
                }
                &parser::StaticValue::Error(ref e, sp) => {
                    self.parser.parser.span_fatal(sp, format!("encountered error while building const_val getter: {}", e).as_str()).emit();
                }
            }
        }

        builder
    }

    /// Builds a getter function for a given Segment.
    ///
    /// The generated function has the signature `pub fn read_{seg_name}() -> {u8, u16, u32}` depending on the Segment info.
    ///
    /// Getters are only generated for segments that can be read.
    fn build_getter(&self, seg: &common::IoRegSegmentInfo, prev_builder: ImplBuilder) -> ImplBuilder {
        let fn_bldr = prev_builder
            .item(format!("read_{}", seg.name)).attr().doc(
                format!("/// Reads the contents (as `{}`) of the `{}` register at relative address 0x{:X}",
                    seg.reg_width.to_type_string(), seg.name, seg.address
            ).as_str())
            .pub_().method().span(seg.span).fn_decl().self_().ref_();
        match seg.reg_width {
            common::RegisterWidth::R8 => {
                fn_bldr.return_().u8().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u8>(seg.address, seg.span))
            }
            common::RegisterWidth::R16 => {
                fn_bldr.return_().u16().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u16>(seg.address, seg.span))
            }
            common::RegisterWidth::R32 => {
                fn_bldr.return_().u32().block()
                    .expr().block().unsafe_()
                        .build_expr(self.build_read_register::<u32>(seg.address, seg.span))
            }
            common::RegisterWidth::Unknown => {
                self.parser.parser.span_fatal(seg.span, "encountered register of unknown size").emit();
                fn_bldr.return_().u32().block().build()
            }
        }
    }

    // TODO: assumes u32 address space
    /// Builds a `volatile_read()` call that reads the entire mcu-proper-register (internally called an Segment).
    ///
    /// For setters that are not fully-byte-aligned, a read is done before the write and the current value is masked
    /// and the re-written with the new bits properly set.
    pub fn build_read_register<T>(&self, addr: u32, sp: Span) -> ptr::P<ast::Expr>
        where T: parser::ToAstType<T>
    {
        let self_offset = self.bldr.expr().span(sp)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(addr as isize)
            .build();

        // volatile_load(self.0.offset(0x1234) as *T)
        self.bldr.expr().span(sp).call().id("volatile_load")
            .with_arg(  ptr_cast!(self_offset, T::to_type(), false)  ) // immutable
            .build()
    }

    /// Builds the skeleton of a `volatile_store()` call, with only the destination location set.
    pub fn build_volatile_store_base<T>(&self, addr: u32, sp: Span) -> FnArgsBuilder
        where T: parser::ToAstType<T>
    {
        let self_offset = self.bldr.expr().span(sp)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(addr as isize)
            .build();

        // volatile_store(self.0.offset(0x1234) as *T, 0x1234)
        self.bldr.stmt().expr().span(sp).call().id("volatile_store")
            .with_arg(ptr_cast!(self_offset, T::to_type(), true)) // mutable
    }

    /// Builds the `volatile_store(dst, src, len)` statement for a setter.
    pub fn build_volatile_store<T>(
        &self, addr: u32, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, v: &common::FunctionValueType
    )
        -> ast::Stmt
        where T: parser::ToAstType<T> + parser::Narrow<u32>
    {
        let base = self.build_volatile_store_base::<T>(addr, fn_def.span);
        match fn_def.ty {
            common::FunctionType::StaticSetter => {
                base.with_arg( T::to_lit(self.get_uint_const_val::<T>(v, seg))  ).build()
            }
            common::FunctionType::Setter => {
                match v {
                    &common::FunctionValueType::Argument(ref arg_name, sp) => {
                        base.with_arg( self.bldr.expr().span(sp).id(arg_name) ).build()
                    }
                    &common::FunctionValueType::Static(_, sp) | &common::FunctionValueType::Reference(_, sp) => {
                        self.parser.parser.span_fatal(sp, "did not expect non-argument type for FunctionType::Setter").emit();
                        base.with_arg( self.bldr.expr().span(sp).id("non_argument_err") ).build()
                    }
                }
            }
            common::FunctionType::Getter => {
                self.parser.parser.span_fatal(fn_def.span, "did not expect getter function for generating volatile_store").emit();
                base.with_arg( self.bldr.expr().span(fn_def.span).id("wrong_func_type") ).build()
            }
        }
    }

    /// Reads into the defined constants for this ioreg and returns the numeric literal behind the constant.
    ///
    /// If not found as a defined internal-constant, a syntax error is placed an 0u32 is returned.
    pub fn get_uint_const_val<T>(&self, v: &common::FunctionValueType, seg: &common::IoRegSegmentInfo) -> T
        where T: parser::ToAstType<T> + parser::Narrow<u32>
    {
        match v {
            &common::FunctionValueType::Static(val, _) => { T::narrow(val) }
            &common::FunctionValueType::Reference(ref name, _) => {
                match self.lookup_const_val(name, seg) {
                    Ok(v) => { expect_uint!(self, v, T) }
                    Err(e) => {
                        panic!(e); // TODO
                    }
                }
            }
            &common::FunctionValueType::Argument(_, sp) => {
                self.parser.parser.span_fatal(sp, "arguments cannot be looked up as constants").emit();
                T::narrow(0u32)
            }
        }
    }


    /// Considers the register, offset, and idex to be written to. Then optimizes and reads/writes needed to achieve the action.
    ///
    /// If writing to a partial register, add a statement that says `let fetched = volatile_load(addr as *const T)`.
    /// Then, we will mask this value and then do a `volatile_store(addr as *mut T, masked | val)`
    ///
    /// TODO: This function is a little heavy duty to be done in a loop -- same responsibilities, but
    ///       perhaps pass in the mask and address so their "expensive" calculations can be reduced
    pub fn optimize_write<T>(&self,
        fn_def: &common::IoRegFuncDef, fn_prev: FnInternalBlockBuilder,
        off: &common::IoRegOffsetIndexInfo, seg: &common::IoRegSegmentInfo
    )
        -> FnInternalBlockBuilder
        where T: parser::ToAstType<T> + parser::Narrow<u32> + BitAnd<T>
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
                let arg_id = self.bldr.expr().span(fn_def.span).id("val");
                let write_val = match off.offset {
                    0 => { arg_id }
                    _ => {
                        self.bldr.expr().span(fn_def.span).paren().build_shl(
                            self.bldr.expr().span(fn_def.span).paren().build_bit_and(
                                arg_id, T::to_lit(T::narrow(val_mask))
                            ),
                            self.bldr.expr().span(fn_def.span).lit().u8(shift_offset)
                        )
                    }
                };

                fn_block = fn_block.with_stmt(
                    self.build_volatile_store_base::<T>(write_address, fn_def.span)
                        .with_arg(
                            self.bldr.expr().span(fn_def.span).build_bit_or(
                                self.bldr.expr().span(fn_def.span).id(mask_id),
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
                            self.bldr.expr().span(fn_def.span).paren().build_shl(
                                T::to_lit( T::narrow( self.get_uint_const_val::<u32>(v, seg) & val_mask )),
                                self.bldr.expr().span(fn_def.span).lit().u8(shift_offset)
                            )
                        }
                    };

                    fn_block = fn_block.with_stmt(
                        self.build_volatile_store_base::<T>(write_address, fn_def.span)
                            .with_arg(
                                self.bldr.expr().span(fn_def.span).build_bit_or(
                                    self.bldr.expr().span(fn_def.span).id(mask_id),
                                    write_val
                                )
                            )
                            .build()
                    );
                }
            }
            common::FunctionType::Getter => {
                self.parser.parser.span_fatal(fn_def.span, "optimize_write did not expect a getter function").emit();
            }
        }

        fn_block
    }


    /// Generates a setter for a given offset+index.
    ///
    /// Given the offset+index type, it will optimize read-before-write unless fully byte aligned.
    /// Also generates documentation for the setter.
    pub fn build_setter<T>(&self, fn_def: &common::IoRegFuncDef, seg: &common::IoRegSegmentInfo, off: &common::IoRegOffsetIndexInfo,  fn_sig: FnInternalBlockBuilder)
        -> ImplBuilder
        where T: parser::ToAstType<T> + parser::Narrow<u32>
    {
        let mut fn_block = fn_sig;

        // if we are setting the entire register, or byte aligned length and index, then write the whole thing blindly :)
        if off.is_fully_byte_aligned() {
            let write_address = seg.address + off.offset_in_bytes();
            match fn_def.ty {
                common::FunctionType::Setter => {
                    let v = common::FunctionValueType::Argument("val".to_string(), DUMMY_SP);
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
                            _ => {
                                self.parser.parser.span_fatal(fn_def.span, "found non-aligned width despite alignment check").emit();
                            }
                        }
                    }
                }
                common::FunctionType::Getter => {
                    self.parser.parser.span_fatal(fn_def.span, "did not expect a getter for generating a setter").emit();
                }
            }
        } else {
            // TODO: how to handle PARTIAL writes to WriteOnly registers?
            if seg.access_perms == common::RegisterPermissions::WriteOnly {
                self.parser.parser.span_fatal(fn_def.span, "partial writes to WriteOnly register indices is currently not supported").emit();
            }

            match off.width {
                1...8 => { fn_block = self.optimize_write::<u8>(fn_def, fn_block, off, seg); }
                9...16 => { fn_block = self.optimize_write::<u16>(fn_def, fn_block, off, seg); }
                17...32 => { fn_block = self.optimize_write::<u32>(fn_def, fn_block, off, seg); }
                _ => { self.parser.parser.span_fatal(fn_def.span, "unknown size for write optimization").emit(); }
            }
        }

        fn_block.build()
    }

    /// Loops the offset+info blocks for this segment and generates all setters.
    pub fn build_setters(&self, off: &common::IoRegOffsetInfo, seg: &common::IoRegSegmentInfo, prev: ImplBuilder)
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
                    self.parser.parser.span_fatal(seg.span, format!("unknown register width to create setter function in segment '{}'", seg.name).as_str()).emit();
                    self.build_setter::<u8>(&f.1, seg, &off.index, ctx) // TODO: ewww, but temporary
                }
            };
        }
        bldr
    }


    /// Gets the StaticValue from the constants defined from this ioreg, with an error if it does not exist.
    ///
    /// **NOTE:** this looks up the constants using the scoped name, but scope prefix is done for you by the function.
    pub fn lookup_const_val<'b>(&'b self, name: &str, seg: &'b common::IoRegSegmentInfo) -> Result<&parser::StaticValue, String> {
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


    /// entry for generating the impl block for the mcu.
    pub fn build_peripheral_init_fns(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let mut blk = impl_block;

        // build needs_init and init
        match &self.reg.init.item {
            &Some(ref i) => {
                blk = blk.item("needs_init").span(self.reg.init.span)
                    .attr().doc(format!("/// states the `{}` needs to be initialized", self.reg.name).as_str())
                    .pub_().method().fn_decl().span(self.reg.init.span)
                        .self_().ref_()
                        .return_().bool()
                        .block()
                            .expr().bool(true);

                blk = blk.with_item(i.clone());
            }
            &None => {
                blk = blk.item("needs_init").span(self.reg.init.span)
                    .attr().doc(format!("/// states the `{}` will not be initialized", self.reg.name).as_str())
                    .pub_().method().fn_decl().span(self.reg.init.span)
                        .self_().ref_()
                        .return_().bool()
                        .block()
                            .expr().bool(false);

                blk = blk.item("init").attr().doc("/// Should not be called as it does nothing. Here to support the init interface")
                    .pub_().method().fn_decl().self_().ref_().default_return()
                    .block().build()
            }
        };

        blk
    }
}
