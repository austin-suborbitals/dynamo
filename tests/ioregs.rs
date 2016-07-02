#![feature(plugin)]
#![plugin(dynamo)]
#[allow(dead_code)]

#[cfg(test)]
mod simple {
    ioreg_debug!(
        // this should not be considered
        name => TestingStruct;      // nor should this

        0x1000 => {
        };
    );

    #[test]
    #[allow(unused_variables)]
    fn struct_is_defined() {
        let x: TestingStruct;
    }
}
