use crate::{
    cart::Cartridge,
    cpu::{self, CPU},
    io::{joypad::Joypad, ppu::LCDController},
    utils::Clock,
};

pub struct Emu {
    pub cpu: CPU,
    clock: Clock,
}

impl Emu {
    pub fn new(cpu: CPU) -> Self {
        Self {
            cpu,
            clock: Default::default(),
        }
    }

    pub fn tick(&mut self) -> cpu::Tick {
        let tick = self.cpu.tick(&self.clock);
        self.clock.tick();
        tick
    }

    pub fn joypad(&mut self) -> &mut Joypad {
        &mut self.cpu.mmu.io.joypad
    }

    pub fn lcd(&self) -> &LCDController {
        &self.cpu.mmu.io.lcd
    }

    pub fn cart(&self) -> Option<&Cartridge> {
        self.cpu.mmu.cartridge.as_ref()
    }
}
