#![feature(plugin)]
#![feature(const_fn)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#![allow(dead_code)]

macro_rules! imports {
    () => {

        extern crate core;
        #[allow(unused_imports)]
        use self::core::intrinsics::{
            volatile_load, volatile_store,
            volatile_set_memory, volatile_copy_nonoverlapping_memory
        };

        #[allow(unused_imports)]
        use std::mem::transmute;
    }
}

#[cfg(test)]
mod sanity {
    imports!();

    mod wdog {
        imports!();
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
    mod uart { imports!(); ioreg!( name => UART; ); }
    mod i2c {  imports!(); ioreg!( name => I2C; ); }

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
    const BSS_BEGIN: u32 = 0xBADC0FFE;
    const BSS_END: u32 = 0xBADFFFFF;

    fn main(_: SomeMcuName) { }
    fn some_common_handler() { }

    mod peregrine { pub mod isr { pub mod default {
		pub static mut some_val: u32 = 0x12345678;
		pub fn some_default_handler() { unsafe{some_val = 0x87654321;} }
	}}}

    mcu!(
        // name given to the generated struct
        name => SomeMcuName;

        // a list of documentation sources.
        // these are added as doc comments to the generated struct for later reference.
        doc_srcs => [
            "http://some.url.com/path/to/probably.pdf",
            "http://some.url.com/path/to/another.pdf"
        ];

        // path to the linker script to use -- NOTE: this is most likely going away
        link_script => "some/path/to/a/linker.ld";

        // when leaving the mcu's init function, this funtion is called within an unsafe block
        // and passed an immutable reference to the mcu initialized in the init function.
        bootloader_exit => main;

        // define constants that will be added to the generated MCU impl block.
        // when added to the block, the key is converted to all caps, but within the macro block
        // can be referenced via lowercase.
        //
        // if you use the uppercase version, it will reference the generated constant, whereas using
        // the lowercase version for values will directly substitute it for the literal. while these
        // should be equivalent to the compiler, ymmv
        constants => {
            i2c_loc = 0x8000;
        };

        // define any externs you want to reference in the generated mcu.
        // a block of the `unsafe "C" { ... }` variety is created, and filled with these values.
        // types for the extern can be an ident or a path.
        externs => {
            data_flash: usize;
            data_flash_end: usize;
            data_section: usize;
        };

        // NOTE: argument to @ _must_ be a link section
        //       the link section _must_ be prefixed with a '.'
        // if you cannot know the link section name at definition-time, you can manually
        // create the array using references to the functions defined in related modules.
        //
        // if no interrupts are defined, no structure is created.
        //
        // NOTE: an error will be generated if you attempt to set ISR0 or ISR1.
        //       these are reserved for the stack pointer and generated reset handler (init) respectively.
        //       these two pointers __must__ be counted in the total number of interrupts, however.
        //
        // interrupts can be literals, idents, paths, or None
        interrupts => [64] @ .interrupts {
            2..5    => some_common_handler;
            6       => peregrine::isr::default::some_default_handler;
			7..32   => None;
        };

        // generate the NVIC interface as if it was an ioreg, but we know the needed functions ahead of time.
        // it will match the dynamo::traits::NVIC trait.
        nvic => {
            addr => 0xE000_E000;    // can be a literal or an ident
            prio_bits => 4;         // must be a literal
        };

        // all values (other than link ptr) can be a literal or ident.
        //
        // these values will be added as constants in the form `{TYPE}_{KEY}` where type is `STACK`, `DATA`, etc
        // and key is the key for the value. for example:
        //      `STACK_BASE` or `HEAP_LIMIT`
        //
        // the data and bss sections do not get constants, however, as they are only used by the mcu's init function.
        memory => {
            // the `base` field requires a link section to place the base pointer
            // this is typically the address of ISR 0.
            //
            // the stack **will not** be located at the link section, simply a pointer to it.
            // it is entirely reasonable to have something like:
            //    `base => .stack_begin @ .stack_ptr`
            // this would place the stack base at `.stack_begin` and create a constant STACK_BEGIN
			//
			// NOTE: the stack link section is going away ASAP. the blocker is casting an address to an
			//       Option<fn()> in a static-compatible way.
            stack => {
                base    => 0x1000;
                limit   => STACK_LIMIT;
            };

            // location of the begin and end of the .data link section (in flash or equivalent).
            // this data must be copied into memory before anything can really happen.
            //
            // `dest` is the address to copy it to.
            data => {
                src_begin   => data_flash;
                src_end     => data_flash_end;
                dest        => data_section;
            };

            // location for any dynamic memory allocation.
            // this is not used by the macro, but is intended for the provided constants to assist in any memory
            // management implementation.
            heap => {
                base    => HEAP_BEGIN;
                limit   => HEAP_END;
            };

            // location of the beginning and end of the .bss section so the init function can null it.
            bss => {
                base    => BSS_BEGIN;
                limit   => BSS_END;
            };
        };

        // a list of peripherals (ioregs, or any MyType(*const u8)) belonging to the mcu.
        // these peripherals are initialized in the order they are defined.
        //
        // as such, it is generally the case that the watchdog must be the first peripheral.
        peripherals => {
            wdog    => wdog::Watchdog @ 0x5000;     // type parsed as a path and pointer set to value after @
            uart    => uart::UART @ UART_1;         // can also be a const/extern
            i2c     => i2c::I2C @ i2c_loc;          // can also be an internal constant
        };

        // a region to specify any additional methods that may be needed.
        // these are parsed wholesale, and should be defined exactly as you wish them to exist in the final
        // impl block for the generated mcu.
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
        assert_eq!(INTERRUPTS.stack, SomeMcuName::STACK_BASE);
        for (i,v) in INTERRUPTS.isrs.into_iter().enumerate() {
            match i {
                0       => {
                    assert!(v.unwrap() == init, "init fn failure");
                }
                1...4   => {
                    assert!(v.unwrap() == some_common_handler, "ident failure");
                }
                5       => {
                    assert!(v.unwrap() == peregrine::isr::default::some_default_handler, "path failure");
                }
                _       => {
                    assert!(v.is_none());
                }
            }
        }
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
    imports!();

    mcu!(
        name => TestMcu;
    );

    fn main(_: TestMcu) {}

    static MCU: TestMcu = TestMcu::new();
    unsafe impl Sync for TestMcu {}

    #[test]
    fn make_static() {}
}


//
// nvic
//

#[cfg(test)]
mod nvic {
    imports!();

    mcu!(
        name => TestMcu;
        nvic => {
            addr => 0xE000_E000;
            prio_bits => 4;
        };
    );

    fn main(_: TestMcu) {}

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

    #[cfg(test)]
    mod with_trait {
        imports!();

        mod traits {
            // standard interface to the Nested Vector Interrupt Controller.
            pub trait NVIC {
                fn enable_irq(&self, irq: u8);
                fn disable_irq(&self, irq: u8);
                fn is_enabled(&self, irq: u8) -> bool;
                fn set_pending(&self, irq: u8);
                fn clear_pending(&self, irq: u8);
                fn is_pending(&self, irq: u8) -> bool;
                fn is_active(&self, irq: u8) -> bool;
                fn set_priority(&self, irq: u8, prio: u8);
                fn get_priority(&self, irq: u8) -> u8;
            }
        }
        fn main(_: TestMcu) {}

        mcu!(
            name => TestMcu;
            nvic => traits::NVIC {
                addr => 0xE000_E000;
                prio_bits => 4;
            };
        );

        fn nvic_handler<T: traits::NVIC>(_: T) -> bool {
            true
        }

        #[test]
        fn generic_handler() {
            assert!(nvic_handler(TestMcu::new().nvic));
        }
    }
}
