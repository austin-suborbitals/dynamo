pub trait Peripheral {
    fn needs_init(&self) -> bool;
    fn init(&self);
}

pub trait MCU {
    fn init(&self);

    fn get_nvic(&self) -> &NVIC;
}

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
