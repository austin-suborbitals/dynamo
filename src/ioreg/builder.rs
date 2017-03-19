extern crate aster;
extern crate num_traits;

use syntax::ast;
use syntax::ptr;
use syntax::print::pprust;

use std::fmt;
use std::collections::BTreeMap;

use common::data;
use common::parser;
use ioreg::common;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;


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
            &parser::StaticValue::Uint(u) => { u.val.into() }

              &parser::StaticValue::Str(_,sp)
            | &parser::StaticValue::Ident(_,_,sp)
            | &parser::StaticValue::Path(_,sp)
            | &parser::StaticValue::Error(_,sp)
            => {
                $self_.parser.parser.span_fatal(sp,
                    "expected a static value after lookup up constant value definition").emit();
                Default::default()
            }
        }
    }
}


macro_rules! try_parse_err {
    ($parser:expr, $sp:expr, $e:expr) => {
        match $e {
            Ok(v) => { v }
            Err(e) => {
                $parser.parser.span_fatal($sp, e.as_str()).emit();
                Default::default()
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
fn make_setter_doc(func: &common::FunctionDef, seg: &common::Segment, reg: &common::Partial) -> Vec<String> {
    let mut result = vec!();
    let mut mask_str = vec!(
        String::from("///"),
        String::from("/// **NOTE:** As an unaligned write, values displayed here are before any shifting or masking occurs."),
    );

    if func.ty == common::FunctionType::Setter {
        result.push(format!(
            "/// Writes the value of the given argument to the `{}` register at relative address 0x{:X} and offset {} bits.",
            seg.name, reg.addr(), reg.offset()
        ));
        if ! reg.slice().is_aligned() { result.append(&mut mask_str) }
        return result;
    }

    let mut actual_vals: Vec<NonQuoteString> = vec!();
    for i in &func.values {
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

    result.push(match actual_vals.len() {
        1 => {
            format!(
                "/// Writes the value `{}` to the `{}` register at relative address 0x{:X} and offset {} bits.",
                actual_vals[0], seg.name, reg.addr(), reg.offset()
            )
        }
        _ => {
            format!(
                "/// Consecutively writes the values `{:?}` to the `{}` register at relative address 0x{:X} and offset {} bits.",
                actual_vals, seg.name, reg.addr(), reg.offset()
            )
        }
    });

    if ! reg.slice().is_aligned() { result.append(&mut mask_str) }
    result
}

/// Builds the AST from the parsed ioreg!() macro.
pub struct Builder<'a> {
    verbose: bool,
    reg: common::IOReg,
    bldr: aster::AstBuilder,
    parser: ::ioreg::parser::Parser<'a>,
}

impl<'a> Builder<'a> {
    /// Consumes the parsed IOReg as well as the parser (for later syntax error placement).
    pub fn new(ioreg: common::IOReg, parser: ::ioreg::parser::Parser, verbose: bool) -> Builder {
        Builder {
            verbose: verbose,
            reg: ioreg,
            bldr: aster::AstBuilder::new(),
            parser: parser,
        }
    }

    /// Outputs Rust AST items representing the generated ioreg.
    pub fn build(&self) -> Vec<ptr::P<ast::Item>> {
        if self.verbose { println!("\n\n=====   Generating: {}   =====\n", self.reg.name); }

        let mut items: Vec<ptr::P<ast::Item>> = vec!();

        // documentation generation for the ioreg struct
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

        // finish generating the base "struct" pointer.
        let type_def = docced_item
            .pub_().tuple_struct(self.reg.name.clone())
                .field().pub_().build_ty( ptr_type!(self.bldr.ty().u8(), false) ) // false = immutable
            .build();
        items.push(type_def);

        // grab the root-level constants to start the impl block
        let mut impl_build = self.bldr.item().span(self.reg.span).impl_();
        impl_build = self.build_assoc_const_vals(&self.reg.const_vals, impl_build);

        // now build the segments
        for s in &self.reg.segments {
            impl_build = self.build_assoc_const_vals(&s.1.const_vals, impl_build);

            // TODO: support manual getter func decl
            if s.1.can_read() {
                // build ::read_foo()
                impl_build = self.build_getter(&s.1, impl_build);
            }

            if s.1.can_write() {
                impl_build = match self.build_setters(&s.1, impl_build) {
                    Ok(i) => { i }
                    Err(_) => { return items; }
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
    fn build_assoc_const_vals(&self, vals: &BTreeMap<String, parser::StaticValue>, prev_build: ImplBuilder) -> ImplBuilder {
        let mut builder = prev_build;
        // generate associated constants in the impl block
        for v in vals {
            let name = v.0.to_uppercase();
            match v.1 {
                &parser::StaticValue::Uint(i) => {
                    builder = builder.item(name).span(i.span)
                        .attr().list("allow").word("dead_code").build()
                        .const_().expr().i32(i.val as i32).ty().i32(); // TODO: narrow not as
                }
                &parser::StaticValue::Str(ref s, sp) => {
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
    /// The generated function has the signature `pub fn read_{seg_name}() -> T` depending on the Segment info.
    ///
    /// Getters are only generated for segments that can be read.
    fn build_getter(&self, seg: &common::Segment, prev_bldr: ImplBuilder) -> ImplBuilder {
        let self_offset = self.bldr.expr().span(seg.span)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(seg.addr().0 as usize)
            .build();

        let fn_sig = prev_bldr
            .item(format!("read_{}", seg.name)).attr().doc(
                format!("/// Reads the contents (as `{}`) of the `{}` register at relative address 0x{:X}",
                    seg.reg.type_str(), seg.name, seg.addr().0
            ).as_str())
            .pub_().method().span(seg.span).fn_decl().self_().ref_();

        match &seg.reg {
            &data::RegisterType::U8(_) => { fn_sig.return_().u8().block() }
            &data::RegisterType::U16(_) => { fn_sig.return_().u16().block() }
            &data::RegisterType::U32(_) => { fn_sig.return_().u32().block() }
            &data::RegisterType::U64(_) => { fn_sig.return_().u64().block() }
        }.expr().block().unsafe_()
            .expr().span(seg.span).call().id("volatile_load") // volatile_load(self.0.offset(0x1234) as *T)
            .with_arg(ptr_cast!(self_offset, seg.reg.to_type(&self.bldr), false)) // immutable
            .build()
    }



    /// Reads into the defined constants for this ioreg and returns the numeric literal behind the constant.
    ///
    /// If not found as a defined internal-constant, a syntax error is placed.
    pub fn get_uint_const_val(&self, v: &common::FunctionValueType, seg: &common::Segment)
        -> Result<data::Unsigned, String>
    {
        match v {
            &common::FunctionValueType::Static(val, _) => {
                Ok(val)
            }
            &common::FunctionValueType::Reference(ref name, sp) => {
                let sv = try!(self.lookup_const_val(name, seg));
                if let &parser::StaticValue::Uint(u) = sv {
                    Ok(u)
                } else {
                    self.parser.parser.span_fatal(sp,
                        "static reference is not a uint").emit();
                    Err(String::from("reference was not a uint"))
                }
            }
            &common::FunctionValueType::Argument(_, sp) => {
                self.parser.parser.span_fatal(sp, "arguments cannot be looked up as constants").emit();
                Err(String::from("did not expect argument type"))
            }
        }
    }


    /// Generate the statements needer to store the passed in value.
    ///
    /// `do_lookup` should be true when the index, offset, or type is unaligned and a load
    /// needs to be generated. This value is recycled across stores as it is masked out.
    pub fn setter_store(
        &self,
        fn_def: &common::FunctionDef,
        seg: &common::Segment, off: &common::Partial,
        write_base: ptr::P<ast::Expr>, do_lookup: bool
    )
        -> Result<Vec<ast::Stmt>, &'static str>
    {
        let mut result: Vec<ast::Stmt> = vec!();
        let mut write_val = write_base;

        // create: `self.0.offset({our offset})`
        let self_offset = self.bldr.expr().span(off.slice().span)
            .method_call("offset").tup_field(0).self_()
            .arg().lit().isize(off.addr())
            .build();

        // create the opening to a vol store: `volatile_store(self.0.offset(0x1234) as *mut T,`
        let mut store_call = self.bldr.stmt().expr().span(fn_def.span).call().id("volatile_store")
            .with_arg(ptr_cast!(self_offset.clone(), off.reg.width_ty(), true)); // true = mutable

        // if not fully aligned, add a vol read and masking to the fn body
        if ! off.slice().is_aligned() {
            let tmp_ident = self.bldr.id("tmp");

            if do_lookup {
                let get_addr = ptr_cast!(
                    self_offset,
                    off.reg.width_ty(),
                    false
                );
                let read_expr = self.bldr.expr().span(seg.span).call().id("volatile_load")
                    .with_arg(get_addr) // immutable
                    .build();

                let read_masked = self.bldr.expr().span(off.slice().span).build_bit_and(
                    read_expr,
                    off.reg.not_mask(&self.bldr)
                );

                let read_set = self.bldr.stmt().span(off.slice().span).build_let(
                    self.bldr.pat().span(off.slice().span).id(tmp_ident.clone()),
                    Some(off.slice().width_ty()),
                    Some(read_masked),
                    vec!()
                );
                result.push(read_set);
            }

            let shift_write = self.bldr.expr().span(off.slice().span).build_shl(
                write_val,
                self.bldr.expr().span(off.slice().span).lit().usize(off.slice().offset)
            );
            let masked_shift = self.bldr.expr().span(off.slice().span).build_bit_and(
                self.bldr.expr().span(off.slice().span).paren().build(shift_write),
                off.reg.mask(&self.bldr)
            );

            write_val = self.bldr.expr().span(off.slice().span).build_bit_or(
                self.bldr.expr().id(tmp_ident.clone()),
                self.bldr.expr().span(off.slice().span).paren().build(masked_shift)
            );
        }

        // do any bit flipping based on the perms we have
        store_call = match seg.reg.perms() {
            // errors
            data::RegisterPermissions::Unknown | data::RegisterPermissions::ReadOnly => {
                self.parser.parser.span_err(seg.span, "encountered register with invalid permissions");
                return Err("invalid register perms");
            }

            // if this is a write-only register, blindly write
            data::RegisterPermissions::WriteOnly => {
                store_call.with_arg(write_val)
            }

            data::RegisterPermissions::ReadWrite => {
                store_call.with_arg(write_val)
            }
        };

        result.push(store_call.build());
        Ok(result)
    }




    /// Generates a setter for a given offset+index.
    ///
    /// Given the offset+index type, it will optimize read-before-write unless fully byte aligned.
    /// Also generates documentation for the setter.
    pub fn build_setter(
        &self,
        fn_def: &common::FunctionDef,
        seg: &common::Segment, off: &common::Partial,
        prev_bldr: ImplBuilder
    )
        -> Result<ImplBuilder, &'static str>
    {

        if ! fn_def.ty.is_write() {
            return Err("cannot build a setter for a non-write fn def");
        }

        let mut item_base = prev_bldr.item(fn_def.name.clone());
        for d in make_setter_doc(fn_def, seg, off) {
            item_base = item_base.attr().doc(d.as_str())
        }
        let mut fn_base = item_base.pub_().method().span(fn_def.span).fn_decl().self_().ref_();

        if fn_def.ty.has_arg() {
            fn_base = fn_base.arg_id("val").with_ty(off.slice().width_ty());
        }

        let mut fn_block = fn_base
            .default_return().block()
            .expr().block().unsafe_();

        for (i, val) in fn_def.values.iter().enumerate() {
            // get+/format the value we are going to write into the representative AST type
            let write_val: ptr::P<ast::Expr> = match val {
                &common::FunctionValueType::Static(u, sp) => {
                    if fn_def.ty != common::FunctionType::StaticSetter {
                        self.parser.parser.span_fatal(sp,
                            "internal error: expected FunctionType::StaticSetter").emit();
                        return Err("invalid argument type");
                    }
                    off.to_lit(&u, &self.bldr)
                }

                &common::FunctionValueType::Argument(ref arg_name, sp) => {
                    if fn_def.ty != common::FunctionType::Setter {
                        self.parser.parser.span_fatal(sp,
                            "internal error: expected FunctionType::Setter").emit();
                        return Err("invalid argument type");
                    }
                    self.bldr.expr().span(sp).id(arg_name)
                }

                &common::FunctionValueType::Reference(ref name, sp) => {
                    if fn_def.ty != common::FunctionType::StaticSetter {
                        self.parser.parser.span_fatal(sp,
                            "internal error: expected FunctionType::StaticSetter").emit();
                        return Err("invalid argument type");
                    }
                    match self.lookup_const_val(name.as_str(), seg) {
                        Ok(v) => {
                            if let &parser::StaticValue::Uint(u) = v {
                                off.to_lit(&u, &self.bldr)
                            } else {
                                self.parser.parser.span_fatal(sp,
                                    "internal error: invalid static value type").emit();
                                return Err("invalid static value type");
                            }
                        }
                        Err(_) => {
                            self.parser.parser.span_fatal(sp, "unknown identifier").emit();
                            return Err("could not find constant reference");
                        }
                    }
                }
            };
            let stmts = try!(self.setter_store(fn_def, seg, off, write_val, i == 0));
            for s in stmts {
                fn_block = fn_block.with_stmt(s);
            }
        }

        if fn_def.ty.has_arg() {
            let stmts = try!(self.setter_store(fn_def, seg, off, self.bldr.expr().span(fn_def.span).id("val"), true));
            for s in stmts {
                fn_block = fn_block.with_stmt(s);
            }
        }

        Ok(fn_block.build())
    }

    /// Loops the offset+info blocks for this segment and generates all setters.
    pub fn build_setters(&self, seg: &common::Segment, prev: ImplBuilder)
        -> Result<ImplBuilder, &'static str>
    {
        let mut bldr = prev;
        for reg in &seg.partials {
            for (_, fn_def) in &reg.functions {
                bldr = try!(self.build_setter(&fn_def, &seg, &reg, bldr));
            }
        }
        Ok(bldr)
    }


    /// Gets the StaticValue from the constants defined from this ioreg, with an error if it does not exist.
    pub fn lookup_const_val<'b>(&'b self, name: &str, seg: &'b common::Segment) -> Result<&parser::StaticValue, String> {
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
