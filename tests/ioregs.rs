#![feature(plugin)]
#![plugin(dynamo)]

#[cfg(test)]
mod simple {
    #[test]
    fn adds_struct() {
        ioreg!()
    }
}
