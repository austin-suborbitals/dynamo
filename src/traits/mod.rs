/// Trait all ioreg (or equivalent) peripherals need to implement if they
/// wish to be handled generically.
pub trait Peripheral {
    /// Truthy value on whether this peripheral needs to be initiated.
    /// This is controlled by the existence of the init keyword and fn in the ioreg definition.
    fn needs_init(&self) -> bool;

    /// Does the work of getting the peripheral into a state where booting can continue.
    /// As much or as little work can be done here as desired.
    fn init(&self);
}

/// Enables generic handling and initialization of an MCU.
pub trait MCU {
    /// Initializes the MCU.
    /// Typically, this entails moving the .data section, clearing the .bss section, initializing
    /// all peripherals that need it, and then jumping out of the bootloader.
    fn init(&self);

    /// Returns the NVIC interface for the MCU.
    /// The NVIC returned must satisfy the NVIC trait.
    fn get_nvic(&self) -> &NVIC;
}

/// Standard interface to the Nested Vector Interrupt Controller.
pub trait NVIC {
    /// Enables the given IRQ.
    fn enable_irq(&self, irq: u8);
    /// Disables the given IRQ.
    fn disable_irq(&self, irq: u8);
    /// Check whether the given IRQ is enabled.
    fn is_enabled(&self, irq: u8) -> bool;

    /// Set the given IRQ as pending, and to be run when priorities dictate.
    fn set_pending(&self, irq: u8);
    /// Clear the given IRQ from the pending list.
    fn clear_pending(&self, irq: u8);
    /// Check whether the given IRQ is pending.
    fn is_pending(&self, irq: u8) -> bool;

    /// Check if the given IRQ is currently being serviced;
    fn is_active(&self, irq: u8) -> bool;

    /// Set the priority of the given IRQ.
    ///
    /// Priorities are limited to the number of priority bits, and this function should handle
    /// all shifting/masking needed.
    fn set_priority(&self, irq: u8, prio: u8);
    /// Gets the priority of the given IRQ.
    ///
    /// Priorities should be returned in the range [0, priority_bits<<2].
    fn get_priority(&self, irq: u8) -> u8;
}
