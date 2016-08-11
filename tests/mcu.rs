#![feature(plugin)]
#![feature(associated_consts)]

#![plugin(dynamo)]

#[allow(dead_code)]

#[cfg(test)]
mod poc {
    mod wdog { ioreg!( name => Watchdog; ); }
    mod uart { ioreg!( name => UART; ); }
    mod i2c { ioreg!( name => I2C; ); }

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
    fn compiles() {}
}
