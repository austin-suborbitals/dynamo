#![feature(plugin)]
#![plugin(dynamo)]
#[allow(dead_code)]

#[cfg(test)]
mod simple {
    ioreg_debug!(
        // this should not be considered
        name => TestingStruct;      // nor should this

        // define some "root-level" constants (unprefixed) on the base struct
        constants => {
            something_important = 0xFFAA;
            less_crucial = 1.123;
            and_a_string = "some super critical string";
        };

        0x1000 => status r16 ro {
            // define some status-related constants. these are prefixed and exist on the segment struct
            constants => {
                some_val = 0x0100;
            };

            0 => {
                using_static => [0x1234, 0x5678, 0x9ABC];
                using_reference => [some_val];
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
