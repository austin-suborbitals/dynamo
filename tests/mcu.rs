#![feature(plugin)]
#![feature(core_intrinsics)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#![allow(dead_code)]

#[cfg(test)]
mod sanity {
    extern crate core;
    use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

    mod wdog { ioreg!( name => Watchdog; ); }
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

    mcu_debug!(
        no_static;

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

        // NOTE: argument to @ _must_ be a link section
        // if you cannot know the link section name at definition-time, you can manually
        // create the array using references to the functions defined in related modules.
        //
        // if no interrupts are defined, no structure is created.
        interrupts => [255] @ .interrupts {
            0       => main;
            1..5    => some_common_handler;
            6       => ::peregrine::isr::default::some_default_handler; // NOTE: paths must start with :: but will not be included
            7..127  => None;
        };

        stack => {
            base    => 0x1000;
            limit   => STACK_LIMIT;             // externally defined const. could also be an internal constant or literal
        };

        data => {
            src_begin   => data_flash;
            src_end     => data_flash_end;
            dest        => data_section;
        };

        heap => {
            base    => HEAP_BEGIN;
            limit   => HEAP_END;
        };

        peripherals => {
            wdog    => wdog::Watchdog @ 0x5000;     // type parsed as a path and pointer set to value after @
            uart    => uart::UART @ UART_1;         // can also be a const/extern
            i2c     => i2c::I2C @ i2c_loc;          // can also be an internal constant
        };
    );


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
}

#[cfg(test)]
mod with_static {
    extern crate core;
    use self::core::intrinsics::volatile_copy_nonoverlapping_memory;

    mcu!(
        name => TestMcu;
    );

    #[test]
    fn instantiated() {
        assert_eq!(MCU, TestMcu::new());
    }

    #[test]
    fn stack_constants_default() {
        assert_eq!(TestMcu::STACK_BASE, 0);
        assert_eq!(TestMcu::STACK_LIMIT, 0);
    }

    #[test]
    fn heap_constants_default() {
        assert_eq!(TestMcu::HEAP_BASE, 0);
        assert_eq!(TestMcu::HEAP_LIMIT, 0);
    }
}
