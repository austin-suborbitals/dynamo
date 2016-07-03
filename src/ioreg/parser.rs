use syntax::ast;
use syntax::parse::token;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::parse::parser as rsparse;

extern crate aster;

pub struct Parser<'a> {
    parser:     rsparse::Parser<'a>,
    curr_span:  Span,
    builder:    aster::AstBuilder,
}

impl<'a> Parser<'a> {
    pub fn from(cx: &mut ExtCtxt<'a>, tree: &[ast::TokenTree]) -> Parser<'a> {
        let p = cx.new_parser_from_tts(tree);
        let s = p.span;
        Parser {
            parser: p,
            curr_span: s,
            builder: aster::AstBuilder::new(),
        }
    }


    //
    // state saving
    //

    fn save_span(&mut self) {
        self.curr_span = self.parser.span;
    }


    //
    // assert and consume
    //

    pub fn expect_ident_value(&mut self, expect: &str) {
        self.save_span();
        match self.parser.parse_ident() {
            Ok(i) => {
                if i.name.as_str() != expect {
                    self.parser.span_err(self.curr_span, format!("expected '{}' but found '{}'", expect, i.name.as_str()).as_str());
                }
            }
            Err(e) => {
                self.parser.span_err(self.curr_span, e.message())
            }
        }
    }

    pub fn expect_semi(&mut self) {
        self.save_span();
        match self.parser.expect(&token::Token::Semi) {
            Ok(_) => {
                // do nothing
            }
            Err(e) => {
                self.parser.span_err(self.parser.span, e.message());
            }
        }
    }


    //
    // simple read
    //

    pub fn get_ident(&mut self) -> ast::Ident {
        self.save_span();
        match self.parser.parse_ident() {
            Ok(i) => { i }
            Err(e) => {
                self.parser.span_err(self.curr_span, e.message());
                ast::Ident::with_empty_ctxt(self.builder.name("error"))
            }
        }
    }

    pub fn get_literal(&mut self) -> ast::Lit {
        self.save_span();
        let lit = match self.parser.parse_lit() {
            Ok(i) => { i }
            Err(e) => {
                // place the error
                self.parser.span_err(self.curr_span, e.message());
                // make a "default" to be ignored
                self.builder.lit().usize(0).unwrap()
            }
        };

        lit
    }


    //
    // read and assert
    //

    pub fn get_int_literal(&mut self) -> ast::Lit {
        let lit = self.get_literal();

        match lit.node {
            ast::LitKind::Int(_,_) => {
                // nothing
            }

            _ => {
                self.parser.span_err(self.curr_span, "expected an unsigned integral literal");
            }
        }

        lit
    }


    //
    // value reading
    //

    pub fn u32_from_lit(&mut self, lit: ast::LitKind) -> u32 {
        if let ast::LitKind::Int(value, _) = lit {
            return value as u32;
        } else {
            panic!("not a u32 lit");
            0
        }
    }

    //
    // passthrough
    //

    pub fn eat(&mut self, tok: &token::Token) {
        self.parser.eat(tok);
    }
}
