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

use parking_lot::Mutex;
use std::{error::Error, ops::ControlFlow};

const M_CLOCK_FREQUENCY: u64 = 4_194_304;
const T_CLOCK_FREQUENCY: u64 = M_CLOCK_FREQUENCY / 4;
const TICK_DURATION: std::time::Duration =
    std::time::Duration::from_nanos(1_000_000_000 / T_CLOCK_FREQUENCY);

const FRAMES_PER_SECOND: u64 = 60;
const NANOSECONDS_PER_SECOND: u64 = 1_000_000_000;
const FRAME_DURATION: std::time::Duration =
    std::time::Duration::from_nanos(NANOSECONDS_PER_SECOND / FRAMES_PER_SECOND);

const TICKS_TO_ADVANCE_PER_RUN_STEP: u32 =
    (T_CLOCK_FREQUENCY / FRAMES_PER_SECOND) as u32;

mod audio;
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
use io::ppu;

use clap::Parser;

fn save_state_with_name(name: &str, cpu: &CPU) {
    let path = std::path::PathBuf::from(name).with_extension("state");
    save_state_from_path(path.to_str().unwrap(), cpu);
}

fn save_state_from_path(path: &str, cpu: &CPU) {
    let mut encoder = flate2::write::GzEncoder::new(
        Vec::new(),
        flate2::Compression::default(),
    );
    ciborium::into_writer(cpu, &mut encoder).unwrap();
    let encoded = encoder.finish().unwrap();
    std::fs::write(path, encoded).unwrap();
}

fn load_state_from_path(path: &str, cpu: &mut CPU) -> bool {
    let Ok(encoded) = std::fs::File::open(path) else {
        return false;
    };
    let decoder = flate2::read::GzDecoder::new(encoded);
    *cpu = ciborium::from_reader(decoder).unwrap();
    cpu.mmu.io.joypad.reset();
    true
}

fn load_state_with_name(name: &str, cpu: &mut CPU) -> bool {
    let path = std::path::PathBuf::from(name).with_extension("state");
    load_state_from_path(path.to_str().unwrap(), cpu)
}

#[derive(Parser)]
struct CliArgs {
    rom_path: String,
    #[arg(long)]
    debug: bool,
    #[arg(long)]
    load_state: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli_args = CliArgs::parse();
    let rom = read_rom(&cli_args.rom_path)?;
    let ctx = {
        let mut cpu = CPU::new();
        if let Some(ref state) = cli_args.load_state {
            load_state_from_path(state, &mut cpu);
        } else {
            cpu.mmu.load_bios(BIOS);
            cpu.mmu.load_cart(rom);
        }
        Box::leak(Box::new(Mutex::new(RunCtx {
            cpu,
            debug: cli_args.debug,
            run_state: if cli_args.debug {
                RunState::Paused
            } else {
                RunState::Running
            },
            //mem_addr: None,
            exit_requested: false,
            speedup_factor: 1.0,
        })))
    };
    std::thread::scope(|s| {
        let ctx = &*ctx;
        let gui_thread = s.spawn(move || {
            gui::main_loop(
                move |event| -> ControlFlow<()> {
                    let mut ctx = ctx.lock();
                    handle_event_(&event, &mut ctx)
                },
                move |ui: &imgui::Ui, frame: imgui::TextureId| {
                    let mut ctx = ctx.lock();
                    draw_ui_(ui, frame, &mut ctx);
                },
                move |frame: &mut gui::ScreenBuffer| {
                    let ctx = ctx.lock();
                    ctx.cpu.mmu.io.lcd.render(frame)
                },
            );
        });
        let audio_thread = s.spawn(move || {
            audio::loop_(
                move |rate: u32| {
                    let mut ctx = ctx.lock();
                    audio_init(&mut ctx, rate);
                },
                move || {
                    let ctx = ctx.lock();
                    audio_tick(&ctx)
                },
                move |data: &mut [f32]| {
                    let mut ctx = ctx.lock();
                    audio_callback(&mut ctx, data);
                },
            )
        });
        let compute_thread = s.spawn(move || compute_thread_(ctx));
        gui_thread.join().unwrap();
        ctx.lock().exit_requested = true;
        compute_thread.join().unwrap();
        audio_thread.join().unwrap();
    });
    Ok(())
}

fn audio_init(ctx: &mut RunCtx, rate: u32) {
    ctx.cpu.mmu.io.apu.set_sample_rate(rate as usize)
}

fn audio_tick(ctx: &RunCtx) -> audio::Action {
    if ctx.exit_requested {
        return audio::Action::Exit;
    }
    match ctx.run_state {
        RunState::Paused | RunState::Stepping(..) => audio::Action::Pause,
        RunState::Running => audio::Action::Play,
    }
}

fn audio_callback(ctx: &mut RunCtx, data: &mut [f32]) {
    ctx.cpu.mmu.io.apu.render(data);
}

fn read_rom(path: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let rom = std::fs::read(path)?;
    if rom.len() < utils::constants::THIRTY_TWO_K {
        return Err("rom length < 32K? {path}")?;
    }
    Ok(rom)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunState {
    Paused,
    Stepping(u32),
    Running,
}

struct RunCtx {
    cpu: CPU,
    debug: bool,
    run_state: RunState,
    //mem_addr: Option<u16>,
    speedup_factor: f32,
    exit_requested: bool,
}

fn draw_ui_(ui: &imgui::Ui, frame: imgui::TextureId, ctx: &mut RunCtx) {
    use crate::utils::Dump;
    let mut dump_str = String::with_capacity(0x1000);
    macro_rules! dump {
        ($e:expr) => {{
            dump_str.clear();
            ($e).dump(&mut dump_str).unwrap();
            &dump_str
        }};
    }
    if ctx.debug {
        const X: usize = 0;
        const Y: usize = 1;
        const DISPLAY_SCALE: f32 = 2.5;
        const DISPLAY_POS: [f32; 2] = [020.0, 000.0];
        const DISPLAY_SIZE: [f32; 2] =
            [DISPLAY_SCALE * 160.0, DISPLAY_SCALE * 144.0];
        fn sum(a: [f32; 2], b: [f32; 2]) -> [f32; 2] {
            [a[0] + b[0], a[1] + b[1]]
        }
        ui.window("display")
            .size(DISPLAY_SIZE, imgui::Condition::Always)
            .position(DISPLAY_POS, imgui::Condition::Always)
            .movable(false)
            .build(|| {
                ui.get_window_draw_list()
                    .add_image(
                        frame,
                        sum(ui.window_pos(), ui.window_content_region_min()),
                        sum(ui.window_pos(), ui.window_content_region_max()),
                    )
                    .build();
            });
        const LCD_POS: [f32; 2] =
            [DISPLAY_POS[X], DISPLAY_POS[Y] + DISPLAY_SIZE[Y]];
        const LCD_SIZE: [f32; 2] = [DISPLAY_SIZE[X] / 2.0, 200.0];
        ui.window("lcd")
            .size(LCD_SIZE, imgui::Condition::FirstUseEver)
            .position(LCD_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text(format!(
                    "TICK RATE: {}",
                    TICKS_TO_ADVANCE_PER_RUN_STEP
                ));
                ui.text_wrapped(dump!(ctx.cpu.mmu.io.lcd));
            })
            .unwrap();
        const JOYPAD_POS: [f32; 2] = [LCD_POS[X] + LCD_SIZE[X], LCD_POS[Y]];
        const JOYPAD_SIZE: [f32; 2] = [DISPLAY_SIZE[X] / 2.0, 100.0];
        ui.window("joypad")
            .size(JOYPAD_SIZE, imgui::Condition::FirstUseEver)
            .position(JOYPAD_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text_wrapped(dump!(ctx.cpu.mmu.io.joypad));
            })
            .unwrap();
        const REGS_POS: [f32; 2] =
            [DISPLAY_POS[X] + DISPLAY_SIZE[X], DISPLAY_POS[Y]];
        const REGS_SIZE: [f32; 2] = [220.0, 300.0];
        ui.window("regs")
            .size(REGS_SIZE, imgui::Condition::FirstUseEver)
            .position(REGS_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text_wrapped(dump!(ctx.cpu));
            })
            .unwrap();
        const CONTROLS_POS: [f32; 2] =
            [REGS_POS[X], REGS_POS[Y] + REGS_SIZE[Y]];
        const CONTROLS_SIZE: [f32; 2] = [REGS_SIZE[X], 300.0];
        ui.window("controls")
            .size(CONTROLS_SIZE, imgui::Condition::FirstUseEver)
            .position(CONTROLS_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                let mut scratch = String::new();
                let bp_changed = ui
                    .input_text("breakpoint", &mut scratch)
                    .enter_returns_true(true)
                    .build();
                if bp_changed {
                    if let Some(addr) = parse_addr(&scratch) {
                        ctx.cpu.breakpoints.register(addr);
                    }
                }
                scratch.clear();
                ui.input_float("speedup", &mut ctx.speedup_factor).build();
                //let mem_changed = ui
                //    .input_text("mem", &mut addr_str)
                //    .enter_returns_true(true)
                //    .build();
                //if mem_changed {
                //    ctx.mem_addr = parse_addr(&addr_str);
                //}
                let breaks = ctx.cpu.breakpoints.dump();
                if !breaks.is_empty() {
                    ui.text(breaks);
                }
                let wp_changed = ui
                    .input_text("watchpoint", &mut scratch)
                    .enter_returns_true(true)
                    .build();
                if wp_changed {
                    if let Some(addr) = parse_addr(&scratch) {
                        ctx.cpu.mmu.register_watchpoint(addr);
                    }
                }
                scratch.clear();
                ctx.cpu.mmu.dump_watchpoints(&mut scratch).unwrap();
                if !scratch.is_empty() {
                    ui.text_wrapped(scratch);
                }
            })
            .unwrap();
        //const MEM_POS: [f32; 2] = [REGS_POS[X] + REGS_SIZE[X], REGS_POS[Y]];
        //const MEM_SIZE: [f32; 2] = [600.0, 400.0];
        //ui.window("mem")
        //    .size(MEM_SIZE, imgui::Condition::FirstUseEver)
        //    .position(MEM_POS, imgui::Condition::FirstUseEver)
        //    .movable(false)
        //    .build(|| {
        //        let start = ctx.mem_addr.unwrap_or(0) as usize;
        //        let block = ctx.cpu.mmu.block_load(start as u16, 0x200);
        //        ui.text(utils::disp_chunks(block, start, 0x10));
        //    })
        //    .unwrap();
        const CART_POS: [f32; 2] =
            [CONTROLS_POS[X] + CONTROLS_SIZE[X], CONTROLS_POS[Y]];
        const CART_SIZE: [f32; 2] = [200.0, 200.0];
        ui.window("cartridge")
            .size(CART_SIZE, imgui::Condition::FirstUseEver)
            .position(CART_POS, imgui::Condition::FirstUseEver)
            .build(|| {
                if let Some(ref cart) = ctx.cpu.mmu.cartridge {
                    ui.text_wrapped(dump!(cart));
                }
            })
            .unwrap();
        const TIMER_POS: [f32; 2] = [LCD_POS[X], LCD_POS[Y] + LCD_SIZE[Y]];
        const TIMER_SIZE: [f32; 2] = [LCD_SIZE[X], 150.0];
        ui.window("timer")
            .size(TIMER_SIZE, imgui::Condition::FirstUseEver)
            .position(TIMER_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text_wrapped(dump!(ctx.cpu.mmu.io.timer));
            })
            .unwrap();
        const DISASM_POS: [f32; 2] = [REGS_POS[X] + REGS_SIZE[X], REGS_POS[Y]];
        const DISASM_SIZE: [f32; 2] = [200.0, 300.0];
        ui.window("disassembly")
            .size(DISASM_SIZE, imgui::Condition::FirstUseEver)
            .position(DISASM_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                // TODO: this is hack: we can't be sure the last instruction
                // boundary was 16 bytes ago, since CB-prefixed instructions
                // are 2-bytes wide.
                let adjusted_pc = ctx.cpu.regs.pc.saturating_sub(16);

                let current_offset = (ctx.cpu.regs.pc - adjusted_pc) as usize;
                let bytes = ctx.cpu.mmu.block_load(adjusted_pc, usize::MAX);
                let insts: Vec<_> = disassembler::InstIter::new(bytes)
                    .map_while(|it| {
                        (it.offset < current_offset
                            || !opcodes::is_unconditional_jump(it.code()))
                        .then(|| {
                            format!(
                                "{}{:04X}: {}",
                                if it.offset == current_offset {
                                    "-->"
                                } else {
                                    "   "
                                },
                                adjusted_pc + (it.offset as u16),
                                it.to_string()
                            )
                        })
                    })
                    .collect();
                ui.text_wrapped(insts.join("\n"));
            })
            .unwrap();
        const APU_POS: [f32; 2] =
            [DISASM_POS[X] + DISASM_SIZE[X], DISASM_POS[Y]];
        const APU_SIZE: [f32; 2] = [300.0, 300.0];
        ui.window("APU")
            .size(APU_SIZE, imgui::Condition::FirstUseEver)
            .position(APU_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text_wrapped(dump!(ctx.cpu.mmu.io.apu));
            })
            .unwrap();
        const LOG_POS: [f32; 2] = [CART_POS[X] + CART_SIZE[X], CART_POS[Y]];
        const LOG_SIZE: [f32; 2] = [400.0, 400.0];
        ui.window("log")
            .size(LOG_SIZE, imgui::Condition::FirstUseEver)
            .position(LOG_POS, imgui::Condition::FirstUseEver)
            .movable(false)
            .build(|| {
                ui.text_wrapped(debug_log_as_str!());
                ui.set_scroll_here_y_with_ratio(1.0);
            })
            .unwrap();
    } else {
        const SCALE: f32 = 4.0;
        ui.window("display")
            .size(
                [SCALE * 160.0 + 50.0, SCALE * 144.0 + 50.0],
                imgui::Condition::Always,
            )
            .position([010.0, 000.0], imgui::Condition::Always)
            .movable(false)
            .build(|| {
                const X_OFFSET: f32 = 25.0;
                const Y_OFFSET: f32 = 25.0;
                let [win_x, win_y] = ui.window_pos();
                ui.get_window_draw_list()
                    .add_image(
                        frame,
                        [win_x + X_OFFSET, win_y + Y_OFFSET],
                        [
                            win_x + SCALE * 160.0 + X_OFFSET,
                            win_y + SCALE * 144.0 + Y_OFFSET,
                        ],
                    )
                    .build();
            });
    }
}

fn parse_addr(break_addr_str: &str) -> Option<u16> {
    if let Some(hex_addr) = break_addr_str.strip_prefix("0x") {
        u16::from_str_radix(hex_addr, 16).ok()
    } else {
        u16::from_str_radix(break_addr_str, 10).ok()
    }
}

const BIOS: &[u8; 0x100] = include_bytes!("../gb_bios.bin");

fn reset_cpu(ctx: &mut RunCtx) {
    ctx.cpu.reset();
    ctx.cpu.mmu.load_bios(BIOS);
}

fn compute_thread_(ctx: &Mutex<RunCtx>) {
    loop {
        let mut ctx = ctx.lock();
        if ctx.exit_requested {
            break;
        }
        let ticks_to_advance = match ctx.run_state {
            RunState::Paused => {
                // When paused, sleep a little bit so we don't busy wait
                std::thread::sleep(std::time::Duration::from_micros(500));
                continue;
            }
            RunState::Stepping(n) => n,
            RunState::Running => TICKS_TO_ADVANCE_PER_RUN_STEP,
        };
        let before_ticks = std::time::Instant::now();
        for _ in 0..ticks_to_advance {
            if ctx.debug && debug::break_::check_and_reset() {
                crate::debug_log!("Hit breakpoint '{0:04X?}'", ctx.cpu.regs.pc);
                ctx.run_state = RunState::Paused;
                break;
            }
            let Tick {
                pc,
                breakpoint_was_hit,
                op_was_retired,
            } = ctx.cpu.tick();
            if ctx.debug {
                if breakpoint_was_hit {
                    ctx.run_state = RunState::Paused;
                    crate::debug_log!("Hit breakpoint '{0:04X?}'", pc);
                    ctx.cpu.breakpoints.reset_count(pc);
                    break;
                }
            }
        }
        match ctx.run_state {
            RunState::Stepping(..) => ctx.run_state = RunState::Paused,
            RunState::Running => {
                let speedup_factor = ctx.speedup_factor;
                // Explicitly drop the mutex before sleeping, so we hold the
                // lock for as little time as possible.
                drop(ctx);
                let expected_tick_time = TICK_DURATION * ticks_to_advance;
                let actual_tick_time = before_ticks.elapsed();
                if expected_tick_time < actual_tick_time {
                    crate::debug_log!(
                        "executing ticks took {}µs longer \
                         than it was supposed to!",
                        (actual_tick_time - expected_tick_time).as_micros()
                    );
                } else {
                    let time_to_sleep = expected_tick_time - actual_tick_time;
                    let slowdown = if speedup_factor == 0.0 {
                        1.0
                    } else {
                        1.0 / speedup_factor
                    };
                    crate::utils::spin_sleep(time_to_sleep.mul_f32(slowdown));
                }
            }
            _ => {}
        }
    }
}

fn handle_event_(event: &gui::Event, ctx: &mut RunCtx) -> ControlFlow<()> {
    use sdl2::keyboard::Keycode as K;

    fn sdl_key_to_joypad_code(key: &sdl2::keyboard::Keycode) -> Option<u8> {
        Some(match key {
            K::Num1 => io::joypad::START,
            K::Num2 => io::joypad::SELECT,
            K::Z => io::joypad::B,
            K::X => io::joypad::A,
            K::Left => io::joypad::LEFT,
            K::Right => io::joypad::RIGHT,
            K::Up => io::joypad::UP,
            K::Down => io::joypad::DOWN,
            _ => return None,
        })
    }

    match event {
        gui::Event::Exit | gui::Event::KeyDown(K::Q) => ControlFlow::Break(()),
        gui::Event::KeyDown(K::Tab) => {
            ctx.debug = !ctx.debug;
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::R) => {
            reset_cpu(ctx);
            debug::log::reset();
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::N) => {
            ctx.run_state = RunState::Stepping(1);
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::M) => {
            ctx.run_state = RunState::Stepping(4);
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::S) => {
            //ctx.run_state = RunState::Stepping(100);
            save_state_with_name(&ctx.cpu.mmu.cart().name(), &ctx.cpu);
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::L) => {
            load_state_with_name(&ctx.cpu.mmu.cart().name(), &mut ctx.cpu);
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(K::Space) => {
            if ctx.run_state == RunState::Running {
                ctx.run_state = RunState::Paused;
            } else {
                ctx.run_state = RunState::Running;
            }
            ControlFlow::Continue(())
        }
        gui::Event::KeyUp(key) => {
            if let Some(code) = sdl_key_to_joypad_code(key) {
                ctx.cpu.mmu.io.joypad.up(code);
            }
            ControlFlow::Continue(())
        }
        gui::Event::KeyDown(key) => {
            if let Some(code) = sdl_key_to_joypad_code(key) {
                ctx.cpu.mmu.io.joypad.down(code);
            }
            ControlFlow::Continue(())
        }
        gui::Event::Unknown(..) => ControlFlow::Continue(()),
    }
}
