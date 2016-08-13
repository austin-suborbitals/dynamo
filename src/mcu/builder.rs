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


macro_rules! integral_or_ident_to_expr {
    ($val:expr, $err:expr, $self_:ident) => {
        match $val {
            StaticValue::Uint(addr, _, sp) => {
                $self_.base_builder.span(sp).expr().lit().u32(addr as u32)
            }
            StaticValue::Int(addr, _, sp) => {
                $self_.base_builder.span(sp).expr().lit().u32(addr as u32)
            }
            StaticValue::Ident(ref id, _, sp) => {
                $self_.base_builder.span(sp).expr().id(id)
            }
            StaticValue::Float(_,_,_,sp) | StaticValue::Str(_,_,sp) | StaticValue::Path(_,sp) | StaticValue::Error(_,sp) => {
                $self_.parser.parser.span_fatal(sp, $err).emit();
                $self_.base_builder.expr().lit().u32(0)
            }
        }
    }
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
        if ! self.mcu.externs.is_empty() {
            result.push(self.build_externs());
        }

        result.push(self.build_struct());
        result.push(self.build_impl());

        if ! self.mcu.interrupts.ints.is_empty() {
            result.push(self.build_interrupts());
        }

        if ! self.mcu.no_static {
            result.push(self.build_sync_impl());
            result.push(self.build_static_instantiation());
        }

        if self.verbose {
            for i in &result { println!("{}\n", pprust::item_to_string(i)); }
        }
        result
    }

    // TODO: lots of clones
    pub fn build_struct(&self) -> ptr::P<ast::Item> {
        // make the struct builder and add the doc attributes
        let mut preamble = self.base_builder.item()
            .attr().list("derive").word("Debug").build()
            .attr().list("derive").word("Clone").build()
            .attr().list("derive").word("PartialEq").build()
            .attr().doc(format!("/// Generated definition of the {} MCU", self.mcu.name).as_str())
            .attr().doc(        "///")
            .attr().doc(        "/// The `Sync` trait is automatically generated, so all locking")
            .attr().doc(        "/// of registers/mcu is left to the implementor")
            .attr().doc(        "///");

        for d in &self.mcu.docs {
            preamble = preamble.attr().doc(format!("/// source: {}", d).as_str());
        }
        if ! self.mcu.link_script.is_empty() {
            preamble = preamble.attr().doc(format!("/// link script: {}", self.mcu.link_script).as_str());
        }

        // and add the peripheral fields
        let mut fields: Vec<ast::StructField> = vec!();
        for p in &self.mcu.peripherals {
            fields.push(self.base_builder.span(p.span).struct_field(p.name.clone()).pub_().ty().build_ty_kind(p.path.clone()));
        }

        preamble.pub_().struct_(self.mcu.name.clone()).with_fields(fields).build()
    }

    pub fn build_sync_impl(&self) -> ptr::P<ast::Item> {
        self.base_builder.item().impl_().unsafe_()
            .trait_().id("Sync").build()
            .ty().id(self.mcu.name.clone())
    }

    pub fn build_new_struct(&self) -> ptr::P<ast::Expr> {
        let mut built_struct = self.base_builder.expr().struct_().id(self.mcu.name.clone()).build();

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

    // TODO: minimize clones
    pub fn build_static_instantiation(&self) -> ptr::P<ast::Item> {
        self.base_builder.item().build_item_kind(
            "MCU",
            ast::ItemKind::Static(
                self.base_builder.ty().id(self.mcu.name.clone()),
                ast::Mutability::Immutable,
                self.build_new_struct()
            )
        )
    }

    // TODO: no cloning the tykind
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

    pub fn build_impl(&self) -> ptr::P<ast::Item> {
        let mut impl_block = self.base_builder.item().impl_();
        impl_block = self.build_const_vals(impl_block);

        // create the ::new() method
        impl_block = impl_block.method("new").span(self.mcu.span).fn_decl()
            .return_().path().id(self.mcu.name.clone()).build()
            .block()
                .build_expr(self.build_new_struct());

        impl_block = self.build_copy_data(impl_block);

        impl_block.ty().id(self.mcu.name.clone())
    }

    pub fn build_copy_data(&self, impl_block: ImplBuilder) -> ImplBuilder {
        // create the ::copy_data_section() method
        let begin_expr = integral_or_ident_to_expr!(self.mcu.data.src_begin, "data src must be a numeric literal or ident", self);
        let end_expr = integral_or_ident_to_expr!(self.mcu.data.src_end, "data src_end must be a numeric literal or ident", self);
        let dest_expr = integral_or_ident_to_expr!(self.mcu.data.dest, "data dest must be a numeric literal or ident", self);

        impl_block.method("copy_data_section").span(self.mcu.data.span).fn_decl()
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

    fn build_const_vals(&self, impl_block: ImplBuilder) -> ImplBuilder {
        let mut builder = impl_block;

        // generate associated constants in the impl block
        for v in &self.mcu.constants {
            let name = v.0.to_uppercase();
            match v.1 {
                &StaticValue::Int(i, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().i32(i).ty().i32();
                }
                &StaticValue::Uint(u, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().u32(u).ty().u32();
                }
                &StaticValue::Float(_, ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr().f32(s).ty().f32();
                }
                &StaticValue::Str(ref s, _, sp) => {
                    builder = builder.item(name).span(sp).const_().expr()
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
                         .const_().with_expr(stack_base_expr).ty().u32();
        builder = builder.item("STACK_LIMIT").span(self.mcu.stack.span)
                         .const_().with_expr(stack_limit_expr).ty().u32();


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

    pub fn build_interrupts(&self) -> ptr::P<ast::Item> {
        let mut ints: Vec<ptr::P<ast::Expr>> = vec![
            self.base_builder.expr().none();
            self.mcu.interrupts.total_ints as usize
        ];
        for i in &self.mcu.interrupts.ints {
            let addr_expr = match &i.1 {
                &StaticValue::Uint(addr, _, sp) => {
                    self.base_builder.expr().span(sp).some().lit().u32(addr)
                }
                &StaticValue::Ident(_, ref id, sp) => {
					if id.name.to_string() == "None".to_string() {
                		self.base_builder.expr().span(sp).none()
					} else {
                		self.base_builder.expr().span(sp).some().id(id)
					}
                }
                &StaticValue::Path(ref path, sp) => {
                	self.base_builder.expr().span(sp).some().build_path(path.clone()) // TODO: clone
                }

                // not allowed
                &StaticValue::Int(_, _,sp) |
                &StaticValue::Float(_,_,_,sp) |
                &StaticValue::Str(_, _,sp) => {
                    self.parser.parser.span_err(sp, "invalid interrupt function location type");
                    self.base_builder.expr().lit().u32(0) // TODO: ew
                }
                &StaticValue::Error(ref err, sp) =>  {
                    self.parser.parser.span_err(sp, format!("unthrown parser error: {}", err).as_str());
                    self.base_builder.expr().lit().u32(0) // TODO: ew
                }
            };

            for i in i.0.begin .. i.0.end+1 {
                ints[i] = addr_expr.clone();    // TODO: this should be faster than building, but check
            }
        }

        self.base_builder.item()
            .attr().name_value("link_section").str(self.mcu.interrupts.link_location.as_str())
            .build_item_kind(
                "INTERRUPTS",
                ast::ItemKind::Static(
                    self.base_builder.ty().build_ty_kind(ast::TyKind::FixedLengthVec(
                        self.base_builder.ty().option().build_ty_kind(ast::TyKind::BareFn(ptr::P(
						ast::BareFnTy {
						    unsafety: ast::Unsafety::Normal, // TODO: or all unsafe?
						    abi: abi::Abi::Rust,
						    lifetimes: vec!(),
						    decl: self.base_builder.fn_decl().default_return(),
                        }))),
                        self.base_builder.expr().lit().usize(self.mcu.interrupts.total_ints as usize)
                    )),
                    ast::Mutability::Immutable,
                    self.base_builder.expr().slice().with_exprs(ints).build()
                )
            )
    }

    pub fn build_stack_and_entry_ptrs(&self) -> Vec<ptr::P<ast::Item>> {
		vec![
		]
	}
}
