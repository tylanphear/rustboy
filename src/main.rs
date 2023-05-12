// Specs:
// CPU: 8-bit
// Main RAM: 8K bytes
// Video RAM: 8K bytes
// Resolution: 160x144 (20x18 tiles)
// Max # of sprites: 40
// Max # sprites/line: 10
// Max sprite size: 8x16
// Min sprite size: 8x8
// Clock speed: 4.194304 MHz
// H-Sync: 9198 KHz
// V-Sync: 59.73 Hz
//
// 1 machine cycle = 4 clock cycles
//         | CPU Speed | NOP Inst
// M-Cycle | 1.05MHz   | 1 cycle
// T-Cycle | 4.19MHz   | 4 cycles

use std::error::Error;
use std::ops::ControlFlow;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

pub mod cart;
pub mod cpu;
pub mod debug;
pub mod disassembler;
mod gui;
pub mod io;
pub mod mmu;
pub mod opcodes;
pub mod utils;

use cpu::{Tick, CPU};
use io::lcd;
use utils::BoundedLog;

fn usage() -> &'static str {
    "usage: <rom-path>"
}

fn main() -> Result<(), Box<dyn Error>> {
    let rom = {
        let path = std::env::args().nth(1).ok_or_else(usage)?;
        read_rom(&path)?
    };
    let ctx = {
        let mut cpu = CPU::new();
        cpu.mmu.load_bios(BIOS);
        cpu.mmu.load_cart(rom);
        Mutex::new(RunCtx {
            cpu,
            ops_to_advance: 0,
            mem_addr: None,
            command_buffer: Default::default(),
        })
    };
    let should_exit = AtomicBool::new(false);
    std::thread::scope(|s| {
        let gui_thread = s.spawn(|| {
            gui::main_loop(
                |event| -> ControlFlow<()> {
                    let mut ctx = ctx.lock().unwrap();
                    handle_event_(&event, &mut ctx)
                },
                |ui: &imgui::Ui, frame: imgui::TextureId| {
                    let mut ctx = ctx.lock().unwrap();
                    draw_ui_(ui, frame, &mut ctx);
                },
                |frame: &mut lcd::ScreenBuffer| {
                    let ctx = ctx.lock().unwrap();
                    ctx.cpu.mmu.io.lcd.draw(frame);
                },
            );
        });
        let compute_thread = s.spawn(|| compute_thread_(&should_exit, &ctx));
        gui_thread.join().unwrap();
        should_exit.store(true, Ordering::Release);
        compute_thread.join().unwrap();
    });
    Ok(())
}

fn read_rom(path: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let rom = std::fs::read(path)?;
    if rom.len() < utils::constants::THIRTY_TWO_K {
        return Err("rom length < 32K? {path}")?;
    }
    Ok(rom)
}

#[derive(Debug)]
struct CommandBuffer {
    buffer: BoundedLog<0x10000, 100>,
    new_added: bool,
}

impl Default for CommandBuffer {
    fn default() -> Self {
        CommandBuffer {
            buffer: Default::default(),
            new_added: false,
        }
    }
}

impl CommandBuffer {
    fn push(&mut self, s: &str) {
        self.buffer.push(s);
        self.new_added = true;
    }

    fn clear(&mut self) {
        self.buffer.clear();
        self.new_added = false;
    }
}

struct RunCtx {
    cpu: CPU,
    ops_to_advance: i64,
    mem_addr: Option<u16>,
    command_buffer: CommandBuffer,
}

fn draw_ui_(ui: &imgui::Ui, frame: imgui::TextureId, ctx: &mut RunCtx) {
    ui.window("command")
        .size([270.0, 500.0], imgui::Condition::FirstUseEver)
        .position([200.0, 000.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            ui.text_wrapped(ctx.command_buffer.buffer.as_str());
            if ctx.command_buffer.new_added {
                ui.set_scroll_here_y_with_ratio(1.0);
                ctx.command_buffer.new_added = false;
            }
        })
        .unwrap();
    ui.window("regs")
        .size([220.0, 200.0], imgui::Condition::FirstUseEver)
        .position([470.0, 000.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let mut regs = String::new();
            ctx.cpu.dump(&mut regs).unwrap();
            ui.text_wrapped(&regs);
        })
        .unwrap();
    ui.window("mem")
        .size([600.0, 500.0], imgui::Condition::FirstUseEver)
        .position([690.0, 000.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let start = ctx.mem_addr.unwrap_or(0) as usize;
            let block = ctx.cpu.mmu.block_load(start as u16, 0x200);
            ui.text(utils::disp_chunks(block, start, 0x10));
        })
        .unwrap();
    ui.window("control")
        .size([220.0, 300.0], imgui::Condition::FirstUseEver)
        .position([470.0, 200.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let mut addr_str = String::new();
            let bp_changed = ui
                .input_text("breakpoint", &mut addr_str)
                .enter_returns_true(true)
                .build();
            if bp_changed {
                if let Some(addr) = parse_addr(&addr_str) {
                    ctx.cpu.breakpoints.register(addr);
                }
                addr_str.clear();
            }
            let mem_changed = ui
                .input_text("mem", &mut addr_str)
                .enter_returns_true(true)
                .build();
            if mem_changed {
                ctx.mem_addr = parse_addr(&addr_str);
            }
            let breaks = ctx.cpu.breakpoints.dump();
            if !breaks.is_empty() {
                ui.text(breaks);
            }
        })
        .unwrap();
    ui.window("display")
        .size([160.0 + 10.0, 144.0 + 10.0], imgui::Condition::FirstUseEver)
        .position([020.0, 000.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let [win_x, win_y] = ui.window_pos();
            ui.get_window_draw_list()
                .add_image(
                    frame,
                    [win_x + 5.0, win_y + 5.0],
                    [win_x + 160.0, win_y + 144.0],
                )
                .build();
        });
    ui.window("lcd")
        .size([180.0, 200.0], imgui::Condition::FirstUseEver)
        .position([020.0, 150.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let mut out = String::new();
            ctx.cpu.mmu.io.lcd.dump(&mut out).unwrap();
            ui.text_wrapped(out);
        })
        .unwrap();
    ui.window("timer")
        .size([200.0, 200.0], imgui::Condition::FirstUseEver)
        .position([000.0, 400.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let mut out = String::new();
            ctx.cpu.mmu.io.timer.dump(&mut out).unwrap();
            ui.text_wrapped(out);
        })
        .unwrap();
    ui.window("log")
        .size([500.0, 200.0], imgui::Condition::FirstUseEver)
        .position([200.0, 500.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            ui.text_wrapped(debug_log_as_str!());
            ui.set_scroll_here_y_with_ratio(1.0);
        })
        .unwrap();
    ui.window("disassembly")
        .size([200.0, 200.0], imgui::Condition::FirstUseEver)
        .position([500.0, 500.0], imgui::Condition::FirstUseEver)
        .movable(false)
        .build(|| {
            let bytes = ctx.cpu.mmu.block_load(ctx.cpu.regs.pc, usize::MAX);
            let insts: Vec<_> =
                disassembler::insts_til_unconditional_jump(bytes)
                    .take(15)
                    .map(|it| it.to_string())
                    .collect();
            ui.text_wrapped(insts.join("\n"));
        })
        .unwrap();
}

fn parse_addr(break_addr_str: &str) -> Option<u16> {
    if break_addr_str.starts_with("0x") {
        u16::from_str_radix(&break_addr_str[2..], 16).ok()
    } else {
        u16::from_str_radix(&break_addr_str, 10).ok()
    }
}

const BIOS: &[u8; 0x100] = include_bytes!("../gb_bios.bin");

fn reset_cpu(ctx: &mut RunCtx) {
    ctx.cpu.reset();
    ctx.command_buffer.clear();
    ctx.cpu.mmu.load_bios(BIOS);
}

fn compute_thread_(should_exit: &AtomicBool, ctx: &Mutex<RunCtx>) {
    use std::fmt::Write;

    let mut scratch_str = String::new();
    macro_rules! f {
        ($($args:tt)*) => {{
            scratch_str.clear();
            writeln!(&mut scratch_str, $($args)*).unwrap();
            &scratch_str
        }}
    }

    while !should_exit.load(Ordering::Relaxed) {
        let mut ctx = ctx.lock().unwrap();
        if ctx.ops_to_advance == 0 {
            continue;
        }
        let Tick {
            pc,
            breakpoint_was_hit,
            op_was_fetched,
            op_was_retired,
        } = ctx.cpu.tick();
        if breakpoint_was_hit || debug::BREAK.swap(false, Ordering::AcqRel) {
            ctx.ops_to_advance = 0;
            ctx.command_buffer.push(f!("Hit breakpoint '{0:04X?}'", pc));
            ctx.cpu.breakpoints.reset(pc);
        }
        if op_was_fetched {
            let RunCtx {
                ref mut cpu,
                ref mut command_buffer,
                ..
            } = *ctx;
            let op = cpu.current_op().unwrap();
            let bytes = cpu.mmu.block_load(pc, op.num_bytes as usize);
            command_buffer.push(f!(
                "{0:04X}: {1} [{2}]",
                pc,
                op.to_string(bytes),
                op.clocks / 4,
            ));
        }
        if op_was_retired {
            if ctx.ops_to_advance > 0 {
                ctx.ops_to_advance = ctx.ops_to_advance.wrapping_sub(1);
            }
        }
    }
}

fn handle_event_(event: &gui::Event, ctx: &mut RunCtx) -> ControlFlow<()> {
    use sdl2::keyboard::Keycode as K;
    match event {
        gui::Event::Exit | gui::Event::Key(K::Q) => ControlFlow::Break(()),
        gui::Event::Key(K::R) => {
            reset_cpu(ctx);
            debug::log::reset();
            ControlFlow::Continue(())
        }
        gui::Event::Key(K::N) => {
            ctx.ops_to_advance = 1 + (ctx.cpu.executing_op() as i64);
            ControlFlow::Continue(())
        }
        gui::Event::Key(K::S) => {
            ctx.ops_to_advance = 100;
            ControlFlow::Continue(())
        }
        gui::Event::Key(K::Space) => {
            if ctx.ops_to_advance == -1 {
                ctx.ops_to_advance = 0
            } else {
                ctx.ops_to_advance = -1;
            }
            ControlFlow::Continue(())
        }
        gui::Event::Key(..) | gui::Event::Unknown(..) => {
            ControlFlow::Continue(())
        }
    }
}
