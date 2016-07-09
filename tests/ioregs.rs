#![feature(plugin)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#[allow(dead_code)]

#[cfg(test)]
mod simple {
    extern crate core;
    use self::core::intrinsics::{volatile_load, volatile_store};

    ioreg_debug!(
        // this should not be considered
        name => TestingStruct;      // nor should this

        // define some "root-level" constants (unprefixed) on the base struct
        constants => {
            // something_signed = -128;     // TODO: need signed ints
            something_important = 0xFFAA;
            less_crucial = 1.123;
            and_a_string = "some super critical string";
        };

        0x1000 => status r16 rw {
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

        0x2000 => control r16 wo {
            0..15 => {
                unlock => [0x00FF, 0x1100];
            }
        };

        0x2000 => accum r16 ro {
            0..15 => {
            }
        };
    );

    #[test]
    #[allow(unused_variables)]
    fn struct_is_defined() {
        let x: TestingStruct;
    }

    #[test]
    fn constants_associated_with_type() {
        //assert_eq!(TestingStruct::SOMETHING_SIGNED, -1);
        assert_eq!(TestingStruct::SOMETHING_IMPORTANT, 0xFFAA);
        assert_eq!(TestingStruct::LESS_CRUCIAL, 1.123);
        assert_eq!(TestingStruct::AND_A_STRING, "some super critical string");
        assert_eq!(TestingStruct::STATUS_SOME_VAL, 0x0100);
    }
}
