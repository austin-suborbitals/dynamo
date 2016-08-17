#![feature(plugin)]
#![feature(const_fn)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#![allow(dead_code)]

#[cfg(test)]
mod sanity {
    extern crate core;
    use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

    mod wdog {
        use sanity::core::intrinsics::{volatile_load, volatile_store};
        ioreg!(
            name => Watchdog;
            0x0000 => unlock r16 rw {
                constants => {
                    value_one = 0x1234;
                    value_two = 0x3456;
                };
                0..15 => { unlock => [value_one, value_two]; }
            };

            0x0000 => control r16 rw {
                7 => { disable => [0x1]; }
            };
        );
    }
    mod uart { ioreg!( name => UART; ); }
    mod i2c { ioreg!( name => I2C; ); }

    // TODO: cannot set the attrs for the entire extern module?
    mod some_extern_thing {
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        #[allow(private_no_mangle_statics)]
        pub static data_flash: usize = 0x1234;
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        #[allow(private_no_mangle_statics)]
        pub static data_flash_end: usize = 0x4321;
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        #[allow(private_no_mangle_statics)]
        pub static data_section: usize = 0xBEEF;
    }

    const UART_1: u32 = 0xDEADBEEF;
    const STACK_LIMIT: u32 = 0x12345678;
    const HEAP_BEGIN: u32 = 0xBADC0FFE;
    const HEAP_END: u32 = 0xBADFFFFF;

	static mut main_value: u32 = 0x12345678;
	static mut common_hndl_value: u32 = 0x12345678;
    fn main(_: &SomeMcuName) { }
    fn some_common_handler() { unsafe{common_hndl_value = 0x87654321;} }

    mod peregrine { pub mod isr { pub mod default {
		pub static mut some_val: u32 = 0x12345678;
		pub fn some_default_handler() { unsafe{some_val = 0x87654321;} }
	}}}

    mcu!(
        name => SomeMcuName;
        doc_srcs => [
            "http://some.url.com/path/to/probably.pdf",
            "http://some.url.com/path/to/another.pdf"
        ];

        link_script => "some/path/to/a/linker.ld";

        constants => {
            i2c_loc = 0x8000;
        };

        externs => {
            data_flash: usize;
            data_flash_end: usize;
            data_section: usize;
        };

		entry_ptr_link => .entry_code_ptr; // link section to place the entry pointer in

        // NOTE: argument to @ _must_ be a link section
        // if you cannot know the link section name at definition-time, you can manually
        // create the array using references to the functions defined in related modules.
        //
        // if no interrupts are defined, no structure is created.
        interrupts => [64] @ .interrupts {
            1..5    => some_common_handler;
            6       => ::peregrine::isr::default::some_default_handler; // NOTE: paths must start with :: but will not be included
			7..32   => None;
        };

        nvic => {
            addr => 0xE000_E000;
            prio_bits => 4;
        };

        stack => {
            base    => 0x1000 @ .stack_base_ptr;
            limit   => STACK_LIMIT;  // externally defined const. could also be an internal constant or literal
        };

        data => {
            // any of the values could be an ident, literal, or path
            src_begin   => data_flash;
            src_end     => data_flash_end;
            dest        => data_section;
        };

        heap => {
            // any of the values could be an ident, literal, or path
            base    => HEAP_BEGIN;
            limit   => HEAP_END;
        };

        peripherals => {
            wdog    => wdog::Watchdog @ 0x5000;     // type parsed as a path and pointer set to value after @
            uart    => uart::UART @ UART_1;         // can also be a const/extern
            i2c     => i2c::I2C @ i2c_loc;          // can also be an internal constant
        };

        actions => [
            pub fn some_helper_function(&self, pin: u8) -> bool {
                return pin > 0x04;
            }
        ];
    );


    #[test]
    fn compiles() {}

    #[test]
    fn correct_periphs() {
        let mcu = SomeMcuName::new();
        assert_eq!(mcu.wdog, wdog::Watchdog(0x5000 as *const u8));
        assert_eq!(mcu.uart, uart::UART(UART_1 as *const u8));
        assert_eq!(mcu.i2c, i2c::I2C(0x8000 as *const u8));
    }

    #[test]
    fn constants() {
        assert_eq!(SomeMcuName::I2C_LOC, 0x8000);
    }

    #[test]
    fn externs() {
        assert_eq!(data_flash, some_extern_thing::data_flash);
        assert_eq!(data_flash_end, some_extern_thing::data_flash_end);
        assert_eq!(data_section, some_extern_thing::data_section);
    }

    #[test]
    fn stack_constants_generated() {
        assert_eq!(SomeMcuName::STACK_BASE, 0x1000);
        assert_eq!(SomeMcuName::STACK_LIMIT, STACK_LIMIT);
    }

    #[test]
    fn heap_constants_generated() {
        assert_eq!(SomeMcuName::HEAP_BASE, HEAP_BEGIN);
        assert_eq!(SomeMcuName::HEAP_LIMIT, HEAP_END);
    }

    #[test]
    fn correct_interrupts() {
		unsafe { // TODO: because of static mut

		for i in 1..6 {
			INTERRUPTS[i].unwrap()();
			assert_eq!(common_hndl_value, 0x87654321);
			common_hndl_value = 0x12345678;
		}

		INTERRUPTS[6].unwrap()();
		assert_eq!(peregrine::isr::default::some_val, 0x87654321);

		} // unsafe
    }

    #[test]
    fn actions() {
        let mcu = SomeMcuName::new();
        assert_eq!(true, mcu.some_helper_function(5));
    }
}

//
// static
//

#[cfg(test)]
mod static_instance {
    extern crate core;
    use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

    mcu!(
        name => TestMcu;
        no_init;
    );

    static MCU: TestMcu = TestMcu::new();
    unsafe impl Sync for TestMcu {}

    #[test]
    fn make_static() {}
}

//
// init
//

#[cfg(test)]
mod init {
    //
    // no init
    //

    #[cfg(test)]
    mod no_init {
        extern crate core;
        use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

        mcu!(
            name => TestMcu;
            no_init;
        );

        static MCU: TestMcu = TestMcu::new();
        unsafe impl Sync for TestMcu {}

        #[test]
        fn compiles() {}
    }


    //
    // generated init
    //

    #[cfg(test)]
    mod generated {
        extern crate core;
        use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

        mod some {
            pub mod module {
                // TODO: make a trait so these can be genericized
                pub fn some_entry_code(_: &super::super::TestMcu) {
                }
            }
        }

        mod wdog {
            use super::core::intrinsics::{volatile_load, volatile_store};
            ioreg!(
                name => Watchdog;
                0x0000 => unlock r16 rw {
                    constants => {
                        value_one = 0x1234;
                        value_two = 0x3456;
                    };
                    0..15 => { unlock => [value_one, value_two]; }
                };

                0x0010 => control r16 rw {
                    7 => { disable => [0x1]; }
                };
            );
        }

        mcu!(
            name => TestMcu;
            peripherals => {
                wdog    => wdog::Watchdog @ 0x5000;
            };
            init => {
                watchdog => wdog;
                exit => some::module::some_entry_code;
            };
        );

        #[test]
        fn compiles() {}
    }
}




//
// nvic
//

#[cfg(test)]
mod nvic {
    extern crate core;
    use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

    mcu!(
        name => TestMcu;
        no_init;

        nvic => {
            addr => 0xE000_E000;
            prio_bits => 4;
        };
    );

    #[test]
    fn correct_addresses() {
        let mcu = TestMcu::new();

        assert_eq!(0xE000_E100, mcu.nvic.iser as u32);
        assert_eq!(0xE000_E180, mcu.nvic.icer as u32);

        assert_eq!(0xE000_E200, mcu.nvic.ispr as u32);
        assert_eq!(0xE000_E280, mcu.nvic.icpr as u32);

        assert_eq!(0xE000_E300, mcu.nvic.iabr as u32);

        assert_eq!(0xE000_E400, mcu.nvic.ipr as u32);
    }

    #[test]
    fn enable_disable_status() {
        // NOTE: this function enables and disables in the same loop, which is unusual, but testable
        let mut enable = [0u32; 8];
        let mut clear = [0u32; 8];

        let mut mcu = TestMcu::new();
        let old_loc = mcu.nvic.iser as u32;

        mcu.nvic.iser = &mut enable as *mut [u32;8];
        assert!(old_loc != mcu.nvic.iser as u32);

        mcu.nvic.icer = &mut clear as *mut [u32;8];
        assert!(old_loc != mcu.nvic.icer as u32);

        for i in 0u8..8u8 { // for every u32 in the "array"
            for b in 0u8..32u8 { // for every bit in the u32
                mcu.nvic.enable_irq((i*32)+b);
                mcu.nvic.disable_irq((i*32)+b);
                assert_eq!(true, mcu.nvic.is_enabled((i*32)+b));
            }

            unsafe { assert_eq!((*mcu.nvic.iser)[i as usize], 0xFFFFFFFF); }
            unsafe { assert_eq!((*mcu.nvic.icer)[i as usize], 0xFFFFFFFF); }
        }
    }

    #[test]
    fn set_and_get_priority() {
        let mut prios = [0u8; 240];

        let mut mcu = TestMcu::new();
        let old_loc = mcu.nvic.ipr as u32;

        mcu.nvic.ipr = &mut prios as *mut [u8;240];
        assert!(old_loc != mcu.nvic.ipr as u32);

        for i in 0u8..240u8 {
            mcu.nvic.set_priority(i, i);
        }

        for i in 0u8..240u8 {
            assert_eq!((i%16), mcu.nvic.get_priority(i));
        }
    }

    #[test]
    fn set_and_get_pending() {
        let mut pends = [0u32; 8];

        let mut mcu = TestMcu::new();
        let old_loc = mcu.nvic.ispr as u32;

        mcu.nvic.ispr = &mut pends as *mut [u32;8];
        assert!(old_loc != mcu.nvic.ispr as u32);

        for i in 0u8..240u8 {
            mcu.nvic.set_pending(i);
            assert_eq!(true, mcu.nvic.is_pending(i));
        }
    }

    #[test]
    fn clear_pending() {
        let mut pends = [0u32; 8];

        let mut mcu = TestMcu::new();
        let old_loc = mcu.nvic.icpr as u32;

        mcu.nvic.icpr = &mut pends as *mut [u32;8];
        assert!(old_loc != mcu.nvic.icpr as u32);

        for i in 0..256u16 { // if set to 255, would not include 255
            mcu.nvic.clear_pending(i as u8);
        }
        for i in 0..8usize {
            assert_eq!(0xFFFFFFFF, pends[i]);
        }
    }
}
