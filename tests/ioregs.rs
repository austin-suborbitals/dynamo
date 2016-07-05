#![feature(plugin)]
#![plugin(dynamo)]
#[allow(dead_code)]

#[cfg(test)]
mod simple {
    ioreg_debug!(
        // this should not be considered
        name => TestingStruct;      // nor should this

        0x1000 => status r16 ro {
            0 => {
                some_val = 0x0100;

                Using_static => [0x1234, 0x5678, 0x9ABC];
                Using_reference => [some_val];
            }

            1 => {

            }

            8..15 => {

            }
        };

        0x2000 => control r8 wr {
            0..15 => {
            }
        };
    );

    #[test]
    #[allow(unused_variables)]
    fn struct_is_defined() {
        let x: TestingStruct;
    }
}
