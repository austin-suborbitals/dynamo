#![feature(plugin)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#[allow(dead_code)]

#[cfg(test)]
mod constants {
    ioreg!(
        // blah blah give it a name
        name => TestingStruct;

        // define some "root-level" constants (unprefixed) on the base struct
        constants => {
            // something_signed = -128;     // TODO: need signed ints
            unsuffixed_hex = 0xFFAA;
            unsuffixed_dec = 1234;
            suffixed_hex = 0xABu8;
            suffixed_dec = 128u8;
            //suffixed_signed_16 = 123i16;
            //suffixed_signed_32 = 456i32;
            unsuffixed_float = 1.123;
            and_a_string = "some super critical string";
        };

        0x0000 => status r16 wo {
            // define some status-related constants. these are prefixed and exist on the segment struct
            constants => {
                nested_const = 0x0100;
            };
        };
    );

    macro_rules! assert_constant { ($name:tt, $val:expr) => { assert_eq!($val, TestingStruct::$name) } }

    #[test]
    fn correct_constants() {
        assert_constant!(UNSUFFIXED_HEX,        0xFFAA);
        assert_constant!(UNSUFFIXED_DEC,        1234);
        assert_constant!(SUFFIXED_HEX,          0xAB);
        assert_constant!(SUFFIXED_DEC,          128);
        //assert_constant!(SUFFIXED_SIGNED_16,    123i16);
        //assert_constant!(SUFFIXED_SIGNED_32,    456i16);
        assert_constant!(UNSUFFIXED_FLOAT,      1.123);
        assert_constant!(AND_A_STRING,          "some super critical string");

        assert_constant!(STATUS_NESTED_CONST,   0x0100);
    }
}


#[cfg(test)]
mod read {
    extern crate core;
    use self::core::intrinsics::{volatile_store, volatile_load};

    ioreg!(
        name => TestingStruct;

        // implicitly testing optional constants{} block

        0x0000 => one_byte r8 ro {
        };

        0x0010 => two_bytes r16 rw {
        };

        0x0020 => four_bytes r32 ro {
        };
    );

    macro_rules! set_correct_val {
        ($reg:expr, $off:expr, $ty:ty, $val:expr) => {
            unsafe { volatile_store(
                (&mut $reg[0] as *mut u8).offset($off) as *mut $ty,     // ((ptr to [0]) + offset) as *mut $ty
                $val
            )};
        }
    }

    macro_rules! setup_test {
        ($off:expr, $exp:expr, $ty:ty) => ({
            let mut reg_mem: [u8; 4096] = [0x48; 4096];
            let t = TestingStruct(&reg_mem as *const u8);
            set_correct_val!(reg_mem, $off, $ty, $exp);
            t
        })
    }

    #[test]
    fn byte_register() {
        let expect: u8 = 0x64;
        let t = setup_test!(0, expect, u8);

        assert_eq!(expect, t.read_one_byte());          // correct
        assert_eq!(0x4848, t.read_two_bytes());         // default value
        assert_eq!(0x48484848, t.read_four_bytes());    // default value
    }

    #[test]
    fn two_byte_register() {
        let expect: u16 = 0x9672;
        let t = setup_test!(0x10, expect, u16);

        assert_eq!(0x48, t.read_one_byte());            // default value
        assert_eq!(expect, t.read_two_bytes());         // correct
        assert_eq!(0x48484848, t.read_four_bytes());    // default value
    }

    #[test]
    fn four_byte_register() {
        let expect: u32 = 0xDEADBEEF;
        let t = setup_test!(0x20, expect, u32);

        assert_eq!(0x48, t.read_one_byte());            // default value
        assert_eq!(0x4848, t.read_two_bytes());         // default_value
        assert_eq!(expect, t.read_four_bytes());        // correct
    }
}


#[cfg(test)]
mod write {
    extern crate core;
    use self::core::intrinsics::{volatile_load, volatile_store};

    ioreg!(
        name => TestingStruct;

        constants => {
            glbl_large_constant = 0x12345678;
        };

        // TODO: make this 'wo' so we have partial writes on a write only register
        0x0000 => split_byte r8 rw {
            0 => {
                set_bit     => [ 1 ];
                clear_bit   => [ 0 ];
            }

            1..3 => {
                inset_and_odd_size => [0xFF]; // _should_ write ((0xFF & 0b0111) << 1)
            }

            4..6 => {
                inset_a_nibble => [0x02];
            }

            7 => {
                set_high_bit => [ 1 ];
                clear_high_bit => [ 0 ];
            }
        };

        0x0010 => full_byte r8 rw {
            0..7 => {
                entire_byte => [0x12];
                repeated_entire_byte => [0x12, 0x34, 0x56];
            }
        };

        0x0020 => two_full_bytes r16 rw {
            0..7 =>  { low_byte => [0x12]; }
            8..15 => { high_byte => [0x34]; }
        };

        0x0030 => two_odd_bytes r16 rw {
            0..9 =>   { first_ten_bits => [ 0x8734 ]; }
            10..15 => { last_six_bits => [ 0xFF ]; }
        };

        0x0040 => double_u16 r32 rw {
            0..15 =>  { first_u16 =>  [0xBEEF]; }
            16..31 => { second_u16 => [0xDEAD]; }
        };

        0x0050 => with_24_bits r32 rw {
            0..23 =>  { set_24_bits => [ 0xDEADBEEF ]; }
        };

        0x0060 => all_but_one_bit r32 rw {
            0..30 =>  { set_31_bits => [ 0xDEADBEEF ]; }
        };

        0x0070 => test_constants r32 rw {
            constants => {
                local_large_constant = 0xDEADBEEF;
            };

            0..7 =>  { glbl_gets_u8_narrowed => [glbl_large_constant]; }
            8..23 =>  { glbl_gets_u16_narrowed => [glbl_large_constant]; }
            24..31 => { local_gets_u8_narrowed => [local_large_constant]; }
        };
    );


    //
    // split byte
    //

    #[test]
    fn set_and_clear_bit() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.set_bit();
        assert_eq!(1, t.read_split_byte());

        t.clear_bit();
        assert_eq!(0, t.read_split_byte());
    }

    #[test]
    fn inset_and_odd_sized() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.inset_and_odd_size();
        assert_eq!(0x0E, t.read_split_byte());
    }

    #[test]
    fn inset_and_nibble_and_odd_size() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.inset_a_nibble();
        assert_eq!(0x20, t.read_split_byte());
    }

    #[test]
    fn set_and_clear_high_bit() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.set_high_bit();
        assert_eq!(0x80, t.read_split_byte());

        t.clear_high_bit();
        assert_eq!(0x00, t.read_split_byte());
    }


    //
    // full byte
    //

    #[test]
    fn set_full_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.entire_byte();
        assert_eq!(0x12, t.read_full_byte());
    }

    #[test]
    fn set_repeated_full_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.repeated_entire_byte();
        assert_eq!(0x56, t.read_full_byte());
    }


    //
    // two full bytes
    //

    #[test]
    fn set_low_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.low_byte();
        assert_eq!(0x0012, t.read_two_full_bytes());
    }

    #[test]
    fn set_high_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.high_byte();
        assert_eq!(0x3400, t.read_two_full_bytes());
    }

    #[test]
    fn set_both_bytes() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.high_byte(); t.low_byte();
        assert_eq!(0x3412, t.read_two_full_bytes());
    }


    //
    // two odd bytes
    //

    #[test]
    fn set_low_gt_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.first_ten_bits();
        assert_eq!(0x0334, t.read_two_odd_bytes());     // only the lower 2 bits of the upper byte
    }

    #[test]
    fn set_high_gt_byte() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.last_six_bits();
        assert_eq!(0xFC00, t.read_two_odd_bytes());     // F -> C as we capture only high 2 bits
    }

    #[test]
    fn set_two_unaligned_adjacent_offsets() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.first_ten_bits(); t.last_six_bits();
        assert_eq!(0xFF34, t.read_two_odd_bytes());
    }


    //
    // two u16 in a r32
    //

    #[test]
    fn set_low_two_bytes() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.first_u16();
        assert_eq!(0xBEEF, t.read_double_u16());
    }

    #[test]
    fn set_high_two_bytes() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.second_u16();
        assert_eq!(0xDEAD0000, t.read_double_u16());
    }

    #[test]
    fn set_both_u16() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.first_u16(); t.second_u16();
        assert_eq!(0xDEADBEEF, t.read_double_u16());
    }


    //
    // with 24 bits
    //

    #[test]
    fn set_24_bit_region() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.set_24_bits();
        assert_eq!(0x00ADBEEF, t.read_with_24_bits());
    }



    //
    // all but one bit
    //

    #[test]
    fn set_31_bit_region() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.set_31_bits();
        assert_eq!(0x5EADBEEF, t.read_all_but_one_bit());
    }


    //
    // constants usage in setter
    //

    #[test]
    fn global_constant_u8_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.glbl_gets_u8_narrowed();
        let expect = TestingStruct::GLBL_LARGE_CONSTANT & 0xFF;
        assert_eq!(0x78, expect);
        assert_eq!(expect, t.read_test_constants());
    }

    #[test]
    fn global_constant_u16_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.glbl_gets_u16_narrowed();
        let expect = (TestingStruct::GLBL_LARGE_CONSTANT & 0xFFFF) << 8;
        assert_eq!(0x567800, expect);
        assert_eq!(expect, t.read_test_constants());
    }

    #[test]
    fn local_constant_u8_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.local_gets_u8_narrowed();
        let expect = TestingStruct::TEST_CONSTANTS_LOCAL_LARGE_CONSTANT << 24;
        assert_eq!(0xEF000000, expect);
        assert_eq!(expect, t.read_test_constants());
    }
}
