#![feature(plugin)]
#![plugin(dynamo)]

#[cfg(test)]
mod simple {
    ioreg!(
        name => TestingStruct
    );

    #[test]
    fn struct_is_defined() {
        let x: TestingStruct;
    }
}
