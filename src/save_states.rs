use std::path::Path;

use flate2::read::DeflateDecoder as Decoder;
use flate2::write::DeflateEncoder as Encoder;

use crate::cpu::CPU;

pub fn save_to_path<P: AsRef<Path>>(cpu: &CPU, path: P) {
    let mut encoder = Encoder::new(Vec::new(), flate2::Compression::fast());
    ciborium::into_writer(cpu, &mut encoder).unwrap();
    let encoded = encoder.finish().unwrap();
    std::fs::write(path, encoded).unwrap();
}

pub fn load_from_path<P: AsRef<Path>>(path: P) -> Option<CPU> {
    let Ok(encoded) = std::fs::File::open(path) else {
        return None;
    };
    let decoder = Decoder::new(encoded);
    let mut cpu: CPU = ciborium::from_reader(decoder).unwrap();
    cpu.mmu.io.joypad.reset();
    Some(cpu)
}

pub fn load_from_slice<T: std::ops::Deref<Target = [u8]>>(data: &T) -> CPU {
    let mut cpu: CPU = ciborium::from_reader(data.deref()).unwrap();
    cpu.mmu.io.joypad.reset();
    cpu
}

pub fn save_to_vec(cpu: &CPU, data: &mut Vec<u8>) {
    data.reserve(std::mem::size_of::<CPU>());
    ciborium::into_writer(cpu, data).unwrap();
}
