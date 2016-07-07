extern crate aster;

use syntax::ast;
use syntax::ptr::P;

use std::collections::HashMap;

use ::ioreg::common;

type ImplBuilder = aster::item::ItemImplBuilder<aster::invoke::Identity>;

pub struct Builder {
    base_builder: aster::AstBuilder,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            base_builder: aster::AstBuilder::new(),
        }
    }

    // TODO: better name?
    // TODO: better return?
    pub fn consume(&mut self, ioreg: &common::IoRegInfo) -> Vec<P<ast::Item>> {
        let mut items: Vec<P<ast::Item>> = vec!();

        // generate the base struct
        items.push(
            self.base_builder.item().pub_().struct_(ioreg.name.clone())
                .build()
        );

        // grab the root-level constants to start the impl block
        let mut impl_build = self.base_builder.item().impl_();
        impl_build = self.build_const_vals(&ioreg.const_vals, impl_build);

        // now do the same for the segments
        for s in &ioreg.segments {
            impl_build = self.build_const_vals(&s.1.const_vals, impl_build);
        }

        // push the impl definition
        items.push(impl_build.ty().id(ioreg.name.clone()));

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
}
