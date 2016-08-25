extern crate aster;

use syntax::ast;
use syntax::ptr;
use syntax::abi;
use syntax::print::pprust;

use ::mcu::common;
use ::mcu::parser;
use ::parser::StaticValue;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;


// TODO: dedupe with ioreg
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

// TODO: dedupe with ioreg
/// Casts the $lhs argument as a pointer of type $rhs and takes a bool for mutability.
macro_rules! ptr_cast {
    ($lhs:expr, $rhs:expr, true) => { ptr_cast!($lhs, $rhs, ast::Mutability::Mutable) };
    ($lhs:expr, $rhs:expr, false) => { ptr_cast!($lhs, $rhs, ast::Mutability::Immutable) };

    ($lhs:expr, $rhs:expr, $mutbl:expr) => { cast!($lhs, ptr_type!($rhs, $mutbl)) };
}

macro_rules! cast {
    ($lhs:expr, $rhs:expr) => {
        aster::AstBuilder::new().expr().span($lhs.span).build_expr_kind(
            ast::ExprKind::Cast( $lhs, $rhs )
        )
    };
}


/// Takes a StaticValue as $val, and converts it to an ast::Expr if compatible.
///
/// Only StaticValue::Uint, StaticValue::Int, and StaticValue::Ident are compatible.
/// All other types will result in a fata syntax error and a dummy 0 value returned.
macro_rules! integral_or_ident_to_expr {
    ($val:expr, $err:expr, $self_:ident) => {
        match $val {
            StaticValue::Uint(addr, _, sp) => {
                $self_.base_builder.expr().span(sp).lit().span(sp).u32(addr as u32)
            }
            StaticValue::Int(addr, _, sp) => {
                $self_.base_builder.expr().span(sp).lit().span(sp).u32(addr as u32)
            }
            StaticValue::Ident(ref id, _, sp) => {
                $self_.base_builder.expr().span(sp).id(id)
            }
            StaticValue::Float(_,_,_,sp) | StaticValue::Str(_,_,sp) | StaticValue::Path(_,sp) | StaticValue::Error(_,sp) => {
                $self_.parser.parser.span_fatal(sp, $err).emit();
                $self_.base_builder.expr().lit().u32(0)
            }
        }
    }
}

/// Takes a StaticValue as $val, and converts it to an ast::Expr as a usize lit (if compatible).
///
/// Only StaticValue::Uint, StaticValue::Int, and StaticValue::Ident are compatible.
/// All other types will result in a fata syntax error and a dummy 0 value returned.
macro_rules! integral_or_ident_to_usize_expr {
    ($val:expr, $err:expr, $self_:ident) => {
        match $val {
            StaticValue::Uint(addr, _, sp) => {
                $self_.base_builder.span(sp).expr().lit().usize(addr as usize)
            }
            StaticValue::Int(addr, _, sp) => {
                $self_.base_builder.span(sp).expr().lit().usize(addr as usize)
            }
            StaticValue::Ident(ref id, _, sp) => {
                $self_.base_builder.span(sp).expr().id(id)
            }
            StaticValue::Float(_,_,_,sp) | StaticValue::Str(_,_,sp) | StaticValue::Path(_,sp) | StaticValue::Error(_,sp) => {
                $self_.parser.parser.span_fatal(sp, $err).emit();
                $self_.base_builder.expr().lit().usize(0)
            }
        }
    }
}

macro_rules! bare_fn {
    ($self_:ident) => {
        $self_.base_builder.ty().build_ty_kind(ast::TyKind::BareFn(ptr::P(
            ast::BareFnTy {
                unsafety: ast::Unsafety::Normal,
                abi: abi::Abi::Rust,
                lifetimes: vec!(),
                decl: $self_.base_builder.fn_decl().default_return(),
            }
        )))
    }
}


/// Builds the generated AST from the parsed mcu!() block.
///
/// Consumes the parser to later set syntax errors.
pub struct Builder<'a> {
    verbose: bool,
    mcu: common::McuInfo,
    base_builder: aster::AstBuilder,
    parser: parser::Parser<'a>,
}

impl<'a> Builder<'a> {
    /// Consumes the parsed common::McuInfo descriptor and parser to construct a Builder.
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
    /// Generates the AST from the consumed descriptor block.
    pub fn build(&self) -> Vec<ptr::P<ast::Item>> {
        let mut result = Vec::<ptr::P<ast::Item>>::new();
        if ! self.mcu.externs.is_empty() {
            result.push(self.build_externs());
        }

        result.push(self.build_nvic_ty());
        result.push(self.build_nvic_impl());
        match &self.mcu.nvic.trait_path {
            &Some(_) => { result.push(self.build_nvic_trait_impl()); }
            &None => {  }
        };

        result.push(self.build_struct());
        result.push(self.build_impl());
        result.push(self.build_init());

        if ! self.mcu.interrupts.ints.is_empty() {
            result.extend(self.build_interrupts());
        }

        if self.verbose {
            for i in &result { println!("{}\n", pprust::item_to_string(i)); }
        }
        result
    }

    // TODO: lots of clones
    /// Builds the MCU structure definition including doc comments, impl functions, etc.
    pub fn build_struct(&self) -> ptr::P<ast::Item> {
        // make the struct builder and add the doc attributes
        let mut preamble = self.base_builder.item()
            .attr().list("repr").word("C").build()
            .attr().list("derive").word("Debug").build()
            .attr().list("derive").word("Clone").build()
            .attr().list("derive").word("PartialEq").build()
            .attr().doc(format!("/// Generated definition of the {} MCU", self.mcu.name).as_str());

        if self.mcu.docs.len() > 1 {
            preamble = preamble.attr().doc("///");
            for d in &self.mcu.docs {
                preamble = preamble.attr().doc(format!("/// source: {}", d).as_str());
            }
        }
        if ! self.mcu.link_script.is_empty() {
            preamble = preamble.attr().doc(format!("/// link script: {}", self.mcu.link_script).as_str());
        }

        // and add the peripheral fields
        let mut fields: Vec<ast::StructField> = vec![
            self.base_builder.struct_field("nvic").pub_().ty().id(format!("{}NVIC",self.mcu.name).as_str()),
        ];
        for p in &self.mcu.peripherals {
            fields.push(self.base_builder.span(p.span).struct_field(p.name.clone()).pub_().ty().build_ty_kind(p.path.clone()));
        }

        preamble.pub_().struct_(self.mcu.name.clone()).with_fields(fields).build()
    }

    /// Generates the structure initializer used in both the ::new() function and to initialize the static version (if generated).
    pub fn build_new_struct(&self) -> ptr::P<ast::Expr> {
        let mut built_struct = self.base_builder.expr()
            .struct_().id(self.mcu.name.clone()).build()
                .field("nvic").build(self.build_nvic_instance());

        // TODO: do this in a .map(|x| x) style if possible
        for p in &self.mcu.peripherals {
            let ty_path = match &p.path {
                &ast::TyKind::Path(_, ref path) => { path.clone() }
                _ => {
                    self.parser.parser.span_err(p.span, format!("cannot use non-path type for peripheral {}", p.name).as_str());
                    self.base_builder.path().id("builder_error").build()
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
                    self.parser.parser.span_err(p.span,
                        format!("must use either a literal or an ident for peripheral address for {}", p.name).as_str());
                }
            }
        }
        built_struct.build()
    }

    // TODO: no cloning the tykind
    /// Generates the `extern "C" { ... }` block for any defined externs in the expansion.
    pub fn build_externs(&self) -> ptr::P<ast::Item> {
        let mut externs: Vec<ast::ForeignItem> = vec!();
        for (k, e) in &self.mcu.externs {
            externs.push(ast::ForeignItem {
                ident: self.base_builder.id(k),
                attrs: vec!(),
                node: ast::ForeignItemKind::Static(self.base_builder.ty().span(e.1).build_ty_kind(e.0.clone()), false),
                id: 0xFFFFFFFF,
                span: e.1,
                vis: ast::Visibility::Inherited,
            });
        }

        self.base_builder.item().build_item_kind("extern_block", ast::ItemKind::ForeignMod(ast::ForeignMod{
            abi: abi::Abi::C,
            items: externs
        }))
    }

    /// entry for generating the impl block for the mcu.
    pub fn build_mcu_init_fn(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let mut blk = impl_block;

        // generate the `init(&self)`
        let mut init_fn = blk.item("init").span(self.mcu.span)
            .attr().doc("/// Initializes the mcu given then memory addresses and peripherals provided to the macro.")
            .attr().doc("///")
            .attr().doc("/// The generated code is as follows:")
            .attr().doc("///")
            .attr().doc("/// ```")
            .attr().doc("/// pub fn init(&self) {")
            .attr().doc("///     self.copy_data_section();")
            .attr().doc("///     self.null_bss();")
            .attr().doc("///     ... for p in periphs ...")
            .attr().doc("///     if p.needs_init() { p.init(); }")
            .attr().doc("///     ...                  ...")
            .attr().doc("///")
            .attr().doc("/// ```")
            .attr().doc("///")
            .attr().doc("/// **NOTE:** peripherals are initialized in the order they are defined")
            .attr().doc("///")
            .attr().doc("/// Each peripheral's `::needs_init()` is a compile time constant, and should not be costly.")
            .pub_().method().fn_decl().span(self.mcu.span)
            .self_().ref_()
            .default_return()
            .block()
                .stmt().span(self.mcu.span).expr().span(self.mcu.span)
                    .method_call("copy_data_section").self_().build()
                .stmt().span(self.mcu.span).expr().span(self.mcu.span)
                    .method_call("null_bss").self_().build();
        for p in &self.mcu.peripherals {
            init_fn = init_fn.stmt().span(p.span).expr().span(p.span)
                .if_().method_call("needs_init").span(p.span).field(p.name.clone()).self_().build()
                .then()
                    .stmt().span(p.span).semi().method_call("init").field(p.name.clone()).self_().build()
                .build().build();
        }
        blk = init_fn.build();

        blk
    }

    /// Entry for generating the impl block for the mcu.
    pub fn build_impl(&self) -> ptr::P<ast::Item> {
        let mut impl_block = self.base_builder.item().impl_();
        impl_block = self.build_const_vals(impl_block);

        // create the ::new() method
        impl_block = impl_block.item("new").span(self.mcu.span)
            .attr().doc("/// Creates a new MCU instance with all peripherals set to the correct addresses.")
            .attr().doc("///")
            .attr().doc("/// This is intended to be used internally, but there is nothing inherently unsafe")
            .attr().doc("/// about creating multiple instances, but thread/access safety still applies.")
            .pub_().method().span(self.mcu.span).const_().span(self.mcu.span).fn_decl()
                .return_().path().id(self.mcu.name.clone()).build()
                .block()
                    .build_expr(self.build_new_struct());

        impl_block = self.build_copy_data(impl_block);
        impl_block = self.build_null_bss(impl_block);
        impl_block = self.build_mcu_init_fn(impl_block);

        // copy the parsed impl items to our new impl
        impl_block = impl_block.with_items(self.mcu.actions.values().map(|a| a.item.clone()));

        impl_block.ty().id(self.mcu.name.clone())
    }

    /// Builds the `::copy_data_section()` function for the MCU definition.
    ///
    /// This function will copy the range [src_begin, src_end] to the data destination location in memory.
    pub fn build_copy_data(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let begin_expr = integral_or_ident_to_expr!(
            self.mcu.data.src_begin, "data src must be a numeric literal or ident", self);
        let end_expr = integral_or_ident_to_expr!(
            self.mcu.data.src_end, "data src_end must be a numeric literal or ident", self);
        let dest_expr = integral_or_ident_to_expr!(
            self.mcu.data.dest, "data dest must be a numeric literal or ident", self);

        impl_block.item("copy_data_section")
            .attr().doc("/// Copies the `.data` section (specified via the `data` block) to main memory")
            .pub_().method().span(self.mcu.data.span).fn_decl()
            .self_().ref_().default_return()
            .block().unsafe_()
                .stmt().expr().span(self.mcu.data.span).call().id("volatile_copy_nonoverlapping_memory")
                    .with_arg(ptr_cast!(dest_expr, self.base_builder.ty().u8(), true /* mut ptr */))
                    .with_arg(ptr_cast!(begin_expr.clone(), self.base_builder.ty().u8(), false /* const ptr */))
                    .with_arg(
                        self.base_builder.expr().build_expr_kind(ast::ExprKind::Cast(
                            self.base_builder.expr().paren().build_sub(end_expr, begin_expr),
                            self.base_builder.ty().usize()
                        ))
                    )
                .build()
            .build()
    }

    /// Builds the `::null_bss()` function for the MCU definition.
    pub fn build_null_bss(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let begin_expr = integral_or_ident_to_expr!(
            self.mcu.bss.base, "bss base must be a numeric literal or ident", self);
        let end_expr = integral_or_ident_to_expr!(
            self.mcu.bss.limit, "bss limit must be a numeric literal or ident", self);

        impl_block.item("null_bss")
            .attr().doc("/// Zeroes all bytes in the `.bss` section as specified in the `bss` block.")
            .pub_().method().span(self.mcu.data.span).fn_decl()
            .self_().ref_().default_return()
            .block().unsafe_()
                .stmt().expr().span(self.mcu.data.span).call().id("volatile_set_memory")
                    .with_arg(ptr_cast!(begin_expr.clone(), self.base_builder.ty().u8(), true /* mut ptr */))
                    .arg().u8(0)
                    .with_arg(cast!(
                        self.base_builder.expr().paren().build_sub(end_expr.clone(), begin_expr.clone()),
                        self.base_builder.ty().usize()
                    ))
                .build()
            .build()
    }

    /// Builds all associated constants for the mcu.
    ///
    /// Idents and Paths cannot be used as constants.
    fn build_const_vals(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let mut builder = impl_block;

        // generate associated constants in the impl block
        for v in &self.mcu.constants {
            let name = v.0.to_uppercase();
            match v.1 {
                &StaticValue::Int(i, _, sp) => {
                    builder = builder.item(name).span(sp).const_().span(sp).expr().i32(i).ty().i32();
                }
                &StaticValue::Uint(u, _, sp) => {
                    builder = builder.item(name).span(sp).const_().span(sp).expr().u32(u).ty().u32();
                }
                &StaticValue::Float(_, ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().span(sp).expr().f32(s).ty().f32();
                }
                &StaticValue::Str(ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().span(sp).expr()
                        .str(s.clone().as_str())
                        .ty().ref_().lifetime("'static").ty().path().id("str").build();
                }
                &StaticValue::Ident(ref s, _, _) => {
                    panic!("cannot use ident as constant: {}", s);
                }
                &StaticValue::Path(ref s, _) => {
                    panic!("cannot use path as constant: {}", s);
                }
                &StaticValue::Error(ref e, _) => {
                    // TODO: what should we do here?
                    panic!("encountered error while building const_val getter: {}", e);
                }
            }
        }

        // build stack constants
        let stack_base_expr = integral_or_ident_to_expr!(
            self.mcu.stack.base, "stack base must be a numeric literal or ident", self);
        let stack_limit_expr = integral_or_ident_to_expr!(
            self.mcu.stack.limit, "stack limit must be a numeric literal or ident", self);
        builder = builder.item("STACK_BASE").span(self.mcu.stack.span)
                         .const_().span(self.mcu.stack.span).with_expr(stack_base_expr).span(self.mcu.stack.span)
                         .ty().span(self.mcu.stack.span).u32();
        builder = builder.item("STACK_LIMIT").span(self.mcu.stack.span)
                         .const_().span(self.mcu.stack.span).with_expr(stack_limit_expr).span(self.mcu.stack.span)
                         .ty().span(self.mcu.stack.span).u32();


        // build heap constants
        let heap_base_expr = integral_or_ident_to_expr!(
            self.mcu.heap.base, "heap base must be a numeric literal or ident", self);
        let heap_limit_expr = integral_or_ident_to_expr!(
            self.mcu.heap.limit, "heap limit must be a numeric literal or ident", self);
        builder = builder.item("HEAP_BASE").span(self.mcu.heap.span)
                         .const_().with_expr(heap_base_expr).ty().u32();
        builder = builder.item("HEAP_LIMIT").span(self.mcu.heap.span)
                         .const_().with_expr(heap_limit_expr).ty().u32();

        builder
    }

    // TODO: insert a generic handler instead of None
    /// Builds the interrupts table and associates the #[link_section = ".some_location"] attribute with it.
    ///
    /// The type is actually a struct containing the stack pointer as a u32 and an array of ISRs.
    /// The array generated is of type `[Option<fn()>; NUM_INTERRUPTS-1]` and defaults to `None` (0u32)
    /// for all interrupts. Making this #[repr(C)] structure keeps type safety while achieving the packed
    /// table structure we need.
    pub fn build_interrupts(&self) -> Vec<ptr::P<ast::Item>> {
        // make an "easy to reference" type InterruptArray that holds ISRs [2, N]
        let int_type = self.base_builder.item().span(self.mcu.interrupts.span)
            .attr().list("repr").word("C").build()
            .struct_("Interrupts")
                .field("stack").span(self.mcu.interrupts.span).ty().u32()
                .field("isrs").span(self.mcu.interrupts.span).ty()
                    .build_ty_kind(ast::TyKind::FixedLengthVec(
                        self.base_builder.ty().option().build(bare_fn!(self)),
                        self.base_builder.expr().lit().usize((self.mcu.interrupts.total_ints-1) as usize)
                    ))
                .build();

        let mut ints: Vec<ptr::P<ast::Expr>> = vec![
            self.base_builder.expr().span(self.mcu.interrupts.span).none();
            (self.mcu.interrupts.total_ints-1) as usize
        ];

        // TODO: I don't really like the -1 stuff because of stack
        // add the entry pointer to the interrupt list
        ints[0] = self.base_builder.expr().some().id("init");

        for i in &self.mcu.interrupts.ints {
            if i.0.contains(0) {
                self.parser.parser.span_fatal(i.1.get_span(),
                    "cannot set an interrupt for ISR0, reserved for stack ptr").emit();
                break;
            }

            if i.0.contains(1) {
                self.parser.parser.span_fatal(i.1.get_span(),
                    "cannot set an interrupt for ISR1, reserved for generated reset handler").emit();
                break;
            }

            let addr_expr = match &i.1 {
                &StaticValue::Ident(_, ref id, sp) => {
					if id.name.to_string() == "None".to_string() {
                        self.base_builder.expr().span(self.mcu.stack.span).none()
					} else {
                		self.base_builder.expr().span(sp).some().id(id)
					}
                }
                &StaticValue::Path(ref path, sp) => {
                    self.base_builder.expr().span(sp).some().build_path(path.clone())
                }

                // not allowed
                &StaticValue::Uint(_, _, sp) |
                &StaticValue::Int(_, _,sp) |
                &StaticValue::Float(_,_,_,sp) |
                &StaticValue::Str(_, _,sp) => {
                    self.parser.parser.span_err(sp, "invalid interrupt function location type");
                    self.base_builder.expr().none() // TODO: ew
                }
                &StaticValue::Error(ref err, sp) =>  {
                    self.parser.parser.span_err(sp, format!("unthrown parser error: {}", err).as_str());
                    self.base_builder.expr().none() // TODO: ew
                }
            };

            for i in i.0.begin .. i.0.end+1 {
                ints[i-1] = addr_expr.clone();    // TODO: this should be faster than building, but check
            }
        }

        vec![
            int_type,
            self.base_builder.item()
                .attr().name_value("link_section").str(self.mcu.interrupts.link_location.as_str())
                .build_item_kind(
                    "INTERRUPTS",
                    ast::ItemKind::Static(
                        self.base_builder.ty().id("Interrupts"),
                        ast::Mutability::Immutable,
                        self.base_builder.expr().span(self.mcu.stack.span).struct_().id("Interrupts").build()
                            .field("stack").build(
                                integral_or_ident_to_expr!(self.mcu.stack.base, "invalid stack base ty", self))
                            .field("isrs").slice().with_exprs(ints).build()
                        .build()
                    )
                )
        ]
    }

    /// Generates the NVIC type we will add to the generated MCU.
    ///
    /// This structure contains the function needed to enable/disable ints, set prios, etc.
    /// It will look and function just like an ioreg peripheral, but generated here instead.
    pub fn build_nvic_ty(&self) -> ptr::P<ast::Item> {
        let ty_name = format!("{}NVIC", self.mcu.name);
        let bitslice = self.base_builder.ty().build_ty_kind(ast::TyKind::FixedLengthVec(
            self.base_builder.ty().u32(),
            self.base_builder.expr().usize(8),
        ));
        let byteslice = self.base_builder.ty().build_ty_kind(ast::TyKind::FixedLengthVec(
            self.base_builder.ty().u8(),
            self.base_builder.expr().usize(240),    // TODO: verify not 256 like others
        ));
        let mut preamble = self.base_builder.item()
            .attr().list("repr").word("C").build()
            .attr().list("derive").word("Clone").build()
            .attr().list("derive").word("Debug").build()
            .attr().list("derive").word("PartialEq").build()
            .attr().doc(format!("/// NVIC interface generated for the {} mcu.", self.mcu.name).as_str())
            .attr().doc("///")
            .attr().doc("/// This structure holds a set of pointers to [u32; 8] slices working as bitmaps.")
            .attr().doc("/// Functions acting on this structure act on the bitmaps themselves.");

        if let Some(ref p) = self.mcu.nvic.trait_path {
            preamble = preamble.attr().doc("///").attr().doc(format!("/// This NVIC satisfies the trait: {}", p).as_str());
        }

        preamble.pub_().struct_(ty_name.as_str())
            .field("iser").pub_().build_ty( ptr_type!(bitslice.clone(), true) )
            .field("icer").pub_().build_ty( ptr_type!(bitslice.clone(), true) )
            .field("ispr").pub_().build_ty( ptr_type!(bitslice.clone(), true) )
            .field("icpr").pub_().build_ty( ptr_type!(bitslice.clone(), true) )
            .field("iabr").pub_().build_ty( ptr_type!(bitslice.clone(), true) )
            .field("ipr").pub_().build_ty(  ptr_type!(byteslice, true) )
        .build()
    }


    /// Generates an `ast::Expr` that describes the instantiation of the NVIC.
    ///
    /// This is used for NVIC::new() as well as the MCU instantiation.
    pub fn build_nvic_instance(&self) -> ptr::P<ast::Expr> {
        let ty_name = format!("{}NVIC", self.mcu.name);
        let bitslice = self.base_builder.ty().build_ty_kind(ast::TyKind::FixedLengthVec(
            self.base_builder.ty().u32(),
            self.base_builder.expr().usize(8),
        ));
        let byteslice = self.base_builder.ty().build_ty_kind(ast::TyKind::FixedLengthVec(
            self.base_builder.ty().u8(),
            self.base_builder.expr().usize(240),    // TODO: verify not 256 like others
        ));

        self.base_builder.expr().span(self.mcu.nvic.span).struct_().id(ty_name.as_str()).build()
            .field("iser").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x100), // TODO: verify offset
                bitslice.clone(),true
            ))
            .field("icer").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x180), // TODO: verify offset
                bitslice.clone(),true
            ))
            .field("ispr").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x200), // TODO: verify offset
                bitslice.clone(),true
            ))
            .field("icpr").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x280), // TODO: verify offset
                bitslice.clone(),true
            ))
            .field("iabr").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x300), // TODO: verify offset
                bitslice.clone(),true
            ))
            .field("ipr").build(ptr_cast!(
                self.base_builder.expr().u32(self.mcu.nvic.addr + 0x400), // TODO: verify offset
                byteslice,true
            ))
            .build()
    }

    // TODO: a good number of assumptions here, but shared across cortex-M apparently
    /// Generates the functions that will exist on the NVIC handler, but not the NVIC trait.
    pub fn build_nvic_impl(&self) -> ptr::P<ast::Item> {
        let ty_name = format!("{}NVIC", self.mcu.name);
        let mut impl_block = self.base_builder.item().span(self.mcu.nvic.span).impl_();

        // make the "::new()" function
        impl_block = impl_block.item("new").span(self.mcu.nvic.span)
            .attr().list("allow").word("dead_code").build()
            .attr().doc("/// Returns a new, and valid, NVIC instantiation.")
            .method().span(self.mcu.nvic.span).const_().fn_decl()
            .return_().id(ty_name.as_str())
            .block()
            .build_expr(self.build_nvic_instance());

        // if we have a trait to satisfy, do not add them to the impl directly
        match &self.mcu.nvic.trait_path {
            &Some(_) => {}
            &None => { impl_block = self.build_nvic_trait_fns(impl_block); }
        }

        impl_block.ty().id(ty_name.as_str())
    }

    pub fn build_nvic_trait_impl(&self) -> ptr::P<ast::Item> {
        let ty_name = format!("{}NVIC", self.mcu.name); // TODO: consolidate this fn/macro to one place
        let mut blk = self.base_builder.item().span(self.mcu.nvic.span).impl_()
            .trait_().build(self.mcu.nvic.trait_path.clone().unwrap());
        blk = self.build_nvic_trait_fns(blk);
        blk.ty().id(ty_name.as_str())
    }

    // TODO: a good number of assumptions here, but shared across cortex-M apparently
    /// Generates the functions that will exist on the NVIC handler.
    pub fn build_nvic_trait_fns(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let mut blk = impl_block;

        //
        // enable and disable
        //

        // make the "::enable_irq(&self, irq: u8)" function
        blk = blk.item("enable_irq").span(self.mcu.nvic.span)
            .attr().doc("/// Enables the given IRQ in the NVIC.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .default_return() // TODO: return a status?
            .block()
                .expr().block().unsafe_()
                .stmt().semi().span(self.mcu.nvic.span).bit_or_assign()
                    .index()
                        .paren().deref().field("iser").self_()
                        .div()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).usize()
                            ))
                            .lit().span(self.mcu.nvic.span).usize(32)
                    .shl()
                        .lit().span(self.mcu.nvic.span).u32(1)
                        .paren().rem()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).u32()
                            ))
                            .lit().span(self.mcu.nvic.span).u32(32)
            .build();


        // make the "::disable_irq(&self, irq: u8)" function
        blk = blk.item("disable_irq").span(self.mcu.nvic.span)
            .attr().doc("/// Disables the given IRQ in the NVIC.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .default_return() // TODO: return a status?
            .block()
                .expr().block().unsafe_()
                .stmt().semi().span(self.mcu.nvic.span).bit_or_assign()
                    .index()
                        .paren().deref().field("icer").self_()
                        .div()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).usize()
                            ))
                            .lit().span(self.mcu.nvic.span).usize(32)
                    .shl()
                        .lit().span(self.mcu.nvic.span).u32(1)
                        .paren().rem()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).u32()
                            ))
                            .lit().span(self.mcu.nvic.span).u32(32)
            .build();

        // make the "::is_enabled(&self, irq: u8) -> bool" function
        blk = blk.item("is_enabled").span(self.mcu.nvic.span)
            .attr().doc("/// Returns whether the given IRQ is enabled or not.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .return_().bool()
            .block()
                .expr().block().unsafe_()
                .expr().span(self.mcu.nvic.span).gt()
                    .bit_and()
                        .index()
                            .paren().deref().field("iser").self_()
                            .div()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).usize()
                                ))
                                .lit().span(self.mcu.nvic.span).usize(32)
                        .shl()
                            .lit().span(self.mcu.nvic.span).u32(1)
                            .paren().rem()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).u32()
                                ))
                                .lit().span(self.mcu.nvic.span).u32(32)
                    .lit().u32(0);

        //
        // pending
        //

        // make the "::set_pending(&self, irq: u8)" function
        blk = blk.item("set_pending").span(self.mcu.nvic.span)
            .attr().doc("/// Sets the given IRQ status to pending in the NVIC.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .default_return() // TODO: return a status?
            .block()
                .expr().block().unsafe_()
                .stmt().semi().span(self.mcu.nvic.span).bit_or_assign()
                    .index()
                        .paren().deref().field("ispr").self_()
                        .div()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).usize()
                            ))
                            .lit().span(self.mcu.nvic.span).usize(32)
                    .shl()
                        .lit().span(self.mcu.nvic.span).u32(1)
                        .paren().rem()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).u32()
                            ))
                            .lit().span(self.mcu.nvic.span).u32(32)
                .build();


        // make the "::clear_pending(&self, irq: u8)" function
        blk = blk.item("clear_pending").span(self.mcu.nvic.span)
            .attr().doc("/// Removes the given IRQ from the pending list.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .default_return() // TODO: return a status?
            .block()
                .expr().block().unsafe_()
                .stmt().semi().span(self.mcu.nvic.span).bit_or_assign()
                    .index()
                        .paren().deref().field("icpr").self_()
                        .div()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).usize()
                            ))
                            .lit().span(self.mcu.nvic.span).usize(32)
                    .shl()
                        .lit().span(self.mcu.nvic.span).u32(1)
                        .paren().rem()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).u32()
                            ))
                            .lit().span(self.mcu.nvic.span).u32(32)
                .build();

        // make the "::is_pending(&self, irq: u8) -> bool" function
        blk = blk.item("is_pending").span(self.mcu.nvic.span)
            .attr().doc("/// Returns whether the given IRQ is pending or not.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .return_().bool()
            .block()
                .expr().block().unsafe_()
                .expr().span(self.mcu.nvic.span).gt()
                    .bit_and()
                        .index()
                            .paren().deref().field("ispr").self_()
                            .div()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).usize()
                                ))
                                .lit().span(self.mcu.nvic.span).usize(32)
                        .shl()
                            .lit().span(self.mcu.nvic.span).u32(1)
                            .paren().rem()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).u32()
                                ))
                                .lit().span(self.mcu.nvic.span).u32(32)
                    .lit().u32(0);

        //
        // active
        //

        // make the "::is_active(&self, irq: u8) -> bool" function
        blk = blk.item("is_active").span(self.mcu.nvic.span)
            .attr().doc("/// Returns whether the given IRQ is actively running or not.")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .return_().bool()
            .block()
                .expr().block().unsafe_()
                .expr().span(self.mcu.nvic.span).gt()
                    .bit_and()
                        .index()
                            .paren().deref().field("iabr").self_()
                            .div()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).usize()
                                ))
                                .lit().span(self.mcu.nvic.span).usize(32)
                        .shl()
                            .lit().span(self.mcu.nvic.span).u32(1)
                            .paren().rem()
                                .build(cast!(
                                    self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                    self.base_builder.ty().span(self.mcu.nvic.span).u32()
                                ))
                                .lit().span(self.mcu.nvic.span).u32(32)
                    .lit().u32(0);

        //
        // priority
        //

        // make the "::set_priority(&self, irq: u8, prio: u8)" function
        blk = blk.item("set_priority").span(self.mcu.nvic.span)
            .attr().doc("/// Sets the given IRQ's priority to the given value in the NVIC.")
            .attr().doc("///")
            .attr().doc("/// **NOTE:** the priority is limited by the priority bits, but all shifts are done for you")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .arg().id("prio").ty().u8()
            .default_return() // TODO: return a status?
            .block()
                .expr().block().unsafe_()
                .stmt().semi().span(self.mcu.nvic.span).assign()
                    .index()
                        .paren().deref().field("ipr").self_()
                        .build(cast!(
                            self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                            self.base_builder.ty().span(self.mcu.nvic.span).usize()
                        ))
                    .shl()
                        .paren().rem()
                            .id("prio")
                            .lit().u8(self.mcu.nvic.prio_bits << 2)
                        .lit().u8(8 - self.mcu.nvic.prio_bits)
                .build();


        // make the "::get_priority(&self, irq: u8) -> u8" function
        blk = blk.item("get_priority").span(self.mcu.nvic.span)
            .attr().doc("/// Gets the given IRQ's priority from the NVIC.")
            .attr().doc("///")
            .attr().doc("/// The returned priority is shifted and will be in the range [0, priot_bits<<2].")
            .method().fn_decl()
            .self_().ref_()
            .arg().id("irq").ty().u8()
            .return_().u8()
            .block()
                .expr().block().unsafe_()
                .expr().span(self.mcu.nvic.span)
                    .shr()
                        .index()
                            .paren().deref().field("ipr").self_()
                            .build(cast!(
                                self.base_builder.expr().span(self.mcu.nvic.span).id("irq"),
                                self.base_builder.ty().span(self.mcu.nvic.span).usize()
                            ))
                        .lit().u8(8 - self.mcu.nvic.prio_bits);


        blk
    }


    // TODO: trigger reset on exit of main
    /// Build the init() function that instantiates the mcu.
    ///
    /// The generated code is as follows:
    ///
    /// ```
    /// pub fn init() {
    ///     let mcu = FooMcu::new();
    ///     mcu.init();
    ///     unsafe { main(mcu); }
    /// }
    /// ```
    ///
    pub fn build_init(&self) -> ptr::P<ast::Item> {
        let exit_bootloader_base = self.base_builder.stmt().span(self.mcu.init.span.clone())
            .expr().span(self.mcu.init.span.clone())
            .block().span(self.mcu.init.span.clone()).unsafe_()
                .stmt().span(self.mcu.init.span.clone()).semi()
                    .call();
        let exit_bootloader = match &self.mcu.init.exit {
            &StaticValue::Path(ref p, sp) => {
                exit_bootloader_base.span(sp.clone()).build_path(p.clone())
            }
            &StaticValue::Ident(_, i, sp) => {
                exit_bootloader_base.span(sp.clone()).id(i.name.to_string())
            }
            _ => {
                exit_bootloader_base.id("INVALID_FN_TYPE_PARSED")
            }
        }.arg().span(self.mcu.init.span.clone()).id("mcu")
              .build()
          .build();

        self.base_builder.item().span(self.mcu.init.span.clone())
            .attr().doc("/// Generated reset handler that creates a new mcu, calls mcu.init(), and calls")
            .attr().doc("/// the function given to `bootloader_exit` with a reference to the mcu instance.")
            .pub_().fn_("init").span(self.mcu.init.span.clone())
            .default_return()
            .block()
                .stmt().span(self.mcu.init.span.clone())
                    .let_id("mcu")
                        .call()
                            .path().span(self.mcu.init.span.clone()).id(self.mcu.name.clone()).id("new").build()
                        .build()
                .stmt().span(self.mcu.init.span.clone()).expr().span(self.mcu.init.span.clone())
                    .method_call("init").span(self.mcu.init.span.clone()).id("mcu").build()
                .with_stmt(exit_bootloader).span(self.mcu.init.span.clone())
            .build()
    }
}
