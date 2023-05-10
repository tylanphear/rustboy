pub mod lcd;
pub mod timer;
use lcd::LCDController;
use timer::Timer;

#[derive(Debug, Default)]
pub struct IOController {
    pub lcd: LCDController,
    pub timer: Timer,
}

impl IOController {
    pub fn new() -> IOController {
        Self::default()
    }

    pub fn reset(&mut self) {
        self.lcd.reset();
        self.timer.reset();
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            0xFF00 =>
            /* todo: joypad */
            {
                0
            }
            0xFF40..=0xFF4B => self.lcd.load(address),
            0xFF10..=0xFF3F =>
            /* sound controller */
            {
                0
            }
            0xFF04..=0xFF07 => self.timer.load(address),
            0xFF00..=0xFF09 => todo!("{address:04X}"),
            _ => panic!("{address:04X}: read outside of IO range!"),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0xFF00 =>
            /* todo: joypad */
            {
                return
            }
            0xFF40..=0xFF4B => self.lcd.store(address, val),
            0xFF10..=0xFF3F =>
            /* sound controller */
            {
                return
            }
            0xFF01..=0xFF02 =>
            /* todo: serial */
            {
                return
            }
            0xFF04..=0xFF07 => self.timer.store(address, val),
            0xFF00..=0xFF09 => {
                todo!("tried to write {val:02X} to {address:04X}")
            }
            _ => panic!("{address:04X} read outside of IO range!"),
        }
    }
}
