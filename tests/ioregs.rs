#![feature(plugin)]
#![plugin(dynamo)]
#[allow(dead_code)]

#[cfg(test)]
mod simple {
    ioreg!(
        name => TestingStruct;
    );

    #[test]
    #[allow(unused_variables)]
    fn struct_is_defined() {
        let x: TestingStruct;
    }
}
