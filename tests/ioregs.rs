#![feature(plugin)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#![allow(dead_code)]

#[cfg(test)]
mod constants {
    extern crate core;
    use self::core::intrinsics::{volatile_load, volatile_store};

    ioreg!(
        // give a name to the generated struct representing the ioreg
        name => TestingStruct;

        // NOTE: omitted here is the init option -- see later tests for details

        // define some "root-level" constants (unprefixed) on the base struct.
        // these constants are available to all scopes in this syntax tree.
        //
        // the constants will be added to the impl block after the key is converted to uppercase.
        // you can, however, use the key as lowercase in which case the constant will not be referenced in
        // generated code, but replaced with the literal.
        constants => {
            unsuffixed_hex = 0xFFAA;
            unsuffixed_dec = 1234;
            suffixed_hex = 0xABu8;
            suffixed_dec = 128u8;
            and_a_string = "some super critical string";
        };

        // this is a segment.
        // most MCUs will consider *this* to be a register.
        //
        // we wish for a logical grouping, so we group the true registers into the relevant grouping.
        // it is entirely reasonable to have an ioreg with only one segment or many.
        //
        // a segment has:
        //     - a name
        //     - a width (r8, r16, r32)
        //     - a permissions (ro, rw, wo)
        //
        // if a segment can be read from (ro, rw) a function is generated to return the value of the segment.
        // the naming convention is `read_{seg_name}()`.
        0x0000 => status r16 rw {
            // segments can also contain constants.
            // these constants are only available within the scope of this segment.
            //
            // when exporting these constants, the segment name followed by a '_' is prepended before
            // conversion to all caps. the below would be `STATUS_NESTED_CONST` for instance.
            constants => {
                nested_const = 0x0100;
            };

            // this is an offset.
            // an offset is a bit or a range of bits within a register.
            //
            // this offset refers to the least significant bit in the segment.
            0 => {
                // this defines a function named `do_a_thing` to be generated in the ioreg's impl.
                // setter functions will sequentially write the values in the brackets to the region.
                //
                // because non-fully-aligned offsets (both in width and offset) cannot be efficiently written,
                // the register is read, the value we will write is then masked and shifted, and the
                // smallest uint that will fit the offset+width is written back.
                //
                // this constraint means we do not support non-byte-aligned writes to write-only registers.
                do_a_thing => [nested_const, nested_const, suffixed_dec];

                // this is the same as above, but with a literal
                do_a_literal_thing => [0x1234];
            }

            // this is also an offset, but with a width.
            // rather than being a single bit, it is multiple contiguous bits.
            // all the same rules apply.
            1..5 => {
                // NOTE: for more details about functions, see the functions suite below.
            }
        };
    );

    macro_rules! assert_constant { ($name:tt, $val:expr) => { assert_eq!($val, TestingStruct::$name) } }

    #[test]
    fn correct_constants() {
        assert_constant!(UNSUFFIXED_HEX,        0xFFAA);
        assert_constant!(UNSUFFIXED_DEC,        1234);
        assert_constant!(SUFFIXED_HEX,          0xAB);
        assert_constant!(SUFFIXED_DEC,          128);
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

        0x0080 => test_multiple_constants r32 rw {
            constants => {
                local_large_constant = 0xDEADBEEF;
                local_other_large_constant = 0xC0FFEE;
            };

            2..9 =>  { multiple_gets_u8_narrowed => [glbl_large_constant, local_other_large_constant]; }
            14..31 =>  { multiple_gets_u32_expanded => [glbl_large_constant, local_other_large_constant]; }
        };

        0x0090 => test_user_input r32 rw {
            0..31 => { set_user_data => (); }
        };

        0x00A0 => test_user_input_unaligned r32 rw {
            3..18 => { set_user_data_unaligned => (); } // 16 bit value, unaligned
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
        assert_eq!(expect as u32, t.read_test_constants());
    }

    #[test]
    fn global_constant_u16_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.glbl_gets_u16_narrowed();
        let expect = (TestingStruct::GLBL_LARGE_CONSTANT & 0xFFFF) << 8;
        assert_eq!(0x567800, expect);
        assert_eq!(expect as u32, t.read_test_constants());
    }

    #[test]
    fn local_constant_u8_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.local_gets_u8_narrowed();
        let expect = TestingStruct::TEST_CONSTANTS_LOCAL_LARGE_CONSTANT << 24;
        assert_eq!(0xEF000000, expect as u32);
        assert_eq!(expect as u32, t.read_test_constants());
    }


    //
    // multiple constants usage in setter
    //

    #[test]
    fn multiple_constant_u8_narrowed() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.multiple_gets_u8_narrowed();
        let expect = (TestingStruct::TEST_MULTIPLE_CONSTANTS_LOCAL_OTHER_LARGE_CONSTANT << 2) & 0xFF;
        assert_eq!(expect as u32, t.read_test_multiple_constants());
    }

    #[test]
    fn multiple_constant_u32_expanded() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.multiple_gets_u32_expanded();
        let expect = TestingStruct::TEST_MULTIPLE_CONSTANTS_LOCAL_OTHER_LARGE_CONSTANT << 14;
        assert_eq!(expect as u32, t.read_test_multiple_constants());
    }


    //
    // user input setters
    //

    #[test]
    fn user_input_setter() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        let expect = 0xDEADBEEF;
        t.set_user_data(expect);
        assert_eq!(expect, t.read_test_user_input());
    }

    #[test]
    fn user_input_setter_unaligned() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        let val = 0xBEEF;
        t.set_user_data_unaligned(val);

        let expect = (val & 0xFFFF) << 3;
        assert_eq!(expect as u32, t.read_test_user_input_unaligned());
    }
}


#[cfg(test)]
mod docs {
    ioreg!(
        name => Multiple;

        // this adds to documentation comments generated for the ioreg struct.
        // each entry is as such:
        //  /// Source: "you string here"
        doc_srcs => ["one", "two", "three"];
    );

    #[test]
    fn compile() {}
}


#[cfg(test)]
mod interchange_doc_const_blocks {
    ioreg!(
        name => DocsFirst;
        doc_srcs => ["test", "other", "third"];
        constants => { a = 0x123; };
    );

    ioreg!(
        name => ConstsFirst;
        constants => { a = 0x123; };
        doc_srcs => ["test", "other", "third"];
    );

    #[test]
    fn works() {}
}

#[cfg(test)]
mod input_fn {
    extern crate core;
    use self::core::intrinsics::{volatile_load, volatile_store};

    ioreg!(
        name => TestingStruct;
        0x0000 => status r16 rw {
            0..4 => {
                // this version of a function definition is special.
                // rather than brackets, it uses parentheses.
                //
                // this signifies the function takes run-time input.
                // the generated fn signature takes one argument, and the type is the smallest uint that
                // can hold the width of the offset.
                this_takes_input => ();
            }


            5..15 => {
                // while it should be rare and is terrible, you can have this
                // style of function as unaligned as you want.
                oh_dear_god => ();
            }
        };

        0x0004 => medium r16 rw {
            4..8 => {
                // this is a typical case
                slide_to_the_left => ();
            }
        };

        0x0008 => big r32 rw {
            7..28 => {
                // not only 1 byte in, but into the last byte without filling it
                whoop => ();
            }
        };
    );


    #[test]
    fn sanity() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.this_takes_input(0x12u8);
        assert_eq!(0x12, t.read_status());
    }

    #[test]
    fn nibble_up() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.slide_to_the_left(0x04u8);
        assert_eq!(0x40, t.read_medium());
    }

    #[test]
    fn offset_and_unaligned() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        t.oh_dear_god(0x1234u16);
        assert_eq!(0x4680, t.read_status());
    }

    #[test]
    fn larger_offset_and_unaligned() {
        let reg_mem: [u8; 4096] = [0; 4096];
        let t = TestingStruct(&reg_mem as *const u8);

        let mut expect = 0xDEADBEEFu32;
        t.whoop(expect);
        expect = (expect << 7/*offset*/) & 0x1FFFFF80/*mask of bits 7-28*/;
        assert_eq!(expect, t.read_big());
    }
}


#[cfg(test)]
mod init {
    static mut TEST_VAL: u16 = 0x1234u16;
    ioreg!(
        name => TestingStruct;

        // if this section is listed, this means the peripheral needs some sort of initialization.
        // everything between the => and the closing semicolon is parsed as a function in an impl block.
        // because of this, your function should look exactly as it should next to generated code -- do not
        // be concerned with scoping here. to use constants do something like Self::MY_CONST.
        //
        // another side-effect of this is another function being generated: needs_init();
        // this function returns a compile-time boolean, making it quick and efficient for the mcu/bootloader
        // to initialize any and all peripherals that need it.
        init =>
        /// While not as elegant to look at, you can totally custom-docstring this guy
        fn init(&self) {
            unsafe { TEST_VAL = 0x4321; }
        };
    );

    #[test]
    fn needs_init() {
        let t = TestingStruct(0 as *mut u8);
        assert!(t.needs_init());
    }

    #[test]
    fn run_init() {
        let t = TestingStruct(0 as *mut u8);
        t.init();
        unsafe { assert_eq!(0x4321, TEST_VAL); }
    }
}

#[cfg(test)]
mod no_init {
    ioreg!(
        name => TestingStruct;
    );

    #[test]
    fn doesnt_need_init() {
        let t = TestingStruct(0 as *mut u8);
        assert_eq!(false, t.needs_init());
    }
}

