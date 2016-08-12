#![feature(plugin)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#[allow(dead_code)]

#[cfg(test)]
mod sanity {
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
        pub static data_section: usize = 0x4321;
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        #[allow(private_no_mangle_statics)]
        pub static data_section_end: usize = 0xBEEF;
    }

    const UART_1: u32 = 0xDEADBEEF;

    mcu!(
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
            data_section: usize;
            data_section_end: usize;
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

        // NOTE: the `base` field requires a link section to place the base pointer -- generally right before the IVT
        //       the stack **will not** be located at the link section, simply a pointer to it.
        //       it is entirely reasonable to have something like:
        //          `base => .stack_begin @ .stack_ptr`
        //       this would place the stack base at `.stack_begin` and create a constant STACK_BEGIN
        stack => {
            base    => 0x1000 @ .stack_ptr;
            limit   => STACK_LIMIT;             // externally defined const. could also be an internal constant or literal
        };

        data => {
            src         => data_flash;
            dest_begin  => data_section;
            dest_end    => data_section_end;
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
        assert_eq!(data_section, some_extern_thing::data_section);
        assert_eq!(data_section_end, some_extern_thing::data_section_end);
    }
}

#[cfg(test)]
mod with_static {
    mcu!(
        name => TestMcu;
    );

    #[test]
    fn instantiated() {
        assert_eq!(MCU, TestMcu::new());
    }
}
