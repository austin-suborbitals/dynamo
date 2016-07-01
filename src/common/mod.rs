extern crate syntax;

use syntax::ast;
use syntax::ptr::P;
use syntax::ext::base::MacResult;
use syntax::util::small_vector::SmallVector;


//
// simple Vec<ast::Item> wrapper that implements MacResult trait
//

pub struct MacItems {
    items: Vec<P<ast::Item>>
}

impl MacItems {
    pub fn new(items: Vec<P<ast::Item>>) -> Box<MacResult+'static> {
        Box::new(MacItems { items: items })
    }
}
impl MacResult for MacItems {
    fn make_items(self: Box<MacItems>) -> Option<SmallVector<P<ast::Item>>> {
        Some(SmallVector::many(self.items.clone()))
    }
}
