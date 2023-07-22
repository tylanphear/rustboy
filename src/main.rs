/// Specs:
/// CPU: 8-bit
/// Main RAM: 8K bytes
/// Video RAM: 8K bytes
/// Resolution: 160x144 (20x18 tiles)
/// Max # of sprites: 40
/// Max # sprites/line: 10
/// Max sprite size: 8x16
/// Min sprite size: 8x8
/// Clock speed: 4.194304 MHz
/// H-Sync: 9198 KHz
/// V-Sync: 59.73 Hz
///
/// 1 machine cycle = 4 clock cycles
///         | CPU Speed | NOP Inst
/// M-Cycle | 1.05MHz   | 1 cycle
/// T-Cycle | 4.19MHz   | 4 cycles
use parking_lot::Mutex;
use std::{error::Error, ops::ControlFlow};

const TICK_DURATION: std::time::Duration = std::time::Duration::from_nanos(
    1_000_000_000 / crate::cpu::M_CLOCK_FREQUENCY,
);

const TICKS_TO_ADVANCE_PER_RUN_STEP: usize =
    (crate::cpu::M_CLOCK_FREQUENCY / 60) as usize;

mod audio;
mod cart;
mod cpu;
mod debug;
mod disassembler;
mod gui;
mod io;
mod mmu;
mod opcodes;
mod save_states;
mod utils;

use cpu::{Tick, CPU};
use io::ppu;

use clap::Parser;

fn save_state_with_name(name: &str, cpu: &CPU) {
    save_states::save_to_path(
        cpu,
        &std::path::PathBuf::from(name).with_extension("state"),
    )
}

fn load_state_with_name(name: &str, cpu: &mut CPU) -> bool {
    let path = std::path::PathBuf::from(name).with_extension("state");
    match save_states::load_from_path(path) {
        Some(saved_cpu) => {
            *cpu = saved_cpu;
            true
        }
        None => false,
    }
}

#[derive(Parser)]
struct CliArgs {
    rom_path: String,
    #[arg(long)]
    debug: bool,
    #[arg(long)]
    load_state: Option<String>,
    #[arg(long)]
    speedup: Option<f32>,
    #[arg(long)]
    volume: Option<f32>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli_args = CliArgs::parse();
    let rom = read_rom(&cli_args.rom_path)?;
    let ctx = {
        let cpu = if let Some(ref state) = cli_args.load_state {
            save_states::load_from_path(state).unwrap()
        } else {
            let mut cpu = CPU::new();
            cpu.mmu.load_bios(BIOS);
            cpu.mmu.load_cart(rom);
            cpu
        };
        Box::leak(Box::new(Mutex::new(RunCtx {
            cpu,
            saved_state: None,
            debug: cli_args.debug,
            run_state: RunState::Running,
            requests: Default::default(),
            speedup: cli_args.speedup.unwrap_or(1.0),
            last_op_address: None,
            volumes: [
                cli_args.volume.unwrap_or(80.0),
                100.0,
                100.0,
                100.0,
                100.0,
            ],
        })))
    };
    std::thread::scope(|s| -> Result<(), Box<dyn Error>> {
        let ctx = &*ctx;
        let gui_thread = s.spawn(move || {
            gui::main_loop(GuiClient { ctx });
        });
        let audio_thread = s.spawn(move || {
            audio::main_loop(
                move |rate: u32, channels: u16| {
                    let mut ctx = ctx.lock();
                    audio_init(&mut ctx, rate, channels);
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
        let compute_thread = s.spawn(|| compute_thread_(ctx));
        gui_thread.join().unwrap();
        ctx.lock().requests.exit = true;
        compute_thread.join().unwrap();
        audio_thread.join().unwrap();

        let debug = debug_log_as_str!().to_string();
        if !debug.is_empty() {
            std::fs::write("debug.log", debug)?;
        }
        Ok(())
    })?;
    Ok(())
}

fn audio_init(ctx: &mut RunCtx, rate: u32, channels: u16) {
    ctx.cpu.mmu.io.apu.init(rate as usize, channels as usize);
}

fn audio_tick(ctx: &RunCtx) -> audio::Action {
    if ctx.requests.exit {
        return audio::Action::Exit;
    }
    match ctx.run_state {
        RunState::Running => audio::Action::Play,
        _ => audio::Action::Pause,
    }
}

fn audio_callback(ctx: &mut RunCtx, data: &mut [f32]) {
    ctx.cpu.mmu.io.apu.render(data, &ctx.volumes);
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
    StepTicks(u32),
    StepOps(u32),
    Running,
}

#[derive(Debug, Default)]
struct Requests {
    exit: bool,
    load_state: bool,
    save_state: bool,
}

struct RunCtx {
    cpu: CPU,
    saved_state: Option<Box<[u8]>>,
    debug: bool,
    run_state: RunState,
    speedup: f32,
    requests: Requests,
    last_op_address: Option<u16>,
    volumes: [f32; 5],
}

fn parse_addr(break_addr_str: &str) -> Option<u16> {
    if let Some(hex_addr) = break_addr_str.strip_prefix("0x") {
        u16::from_str_radix(hex_addr, 16).ok()
    } else {
        u16::from_str_radix(break_addr_str, 10).ok()
    }
}

const BIOS: &[u8; 0x100] = include_bytes!("../gb_bios.bin");

fn reset_cpu(cpu: &mut CPU) {
    cpu.reset();
    cpu.mmu.load_bios(BIOS);
}

fn compute_thread_(ctx: &Mutex<RunCtx>) {
    let mut last_tick_time;
    let mut last_save_time = std::time::Instant::now();
    loop {
        let mut ctx = ctx.lock();
        if ctx.requests.exit {
            ctx.cpu.mmu.cart().dump_sram();
            break;
        }
        if std::mem::take(&mut ctx.requests.save_state) {
            save_state_with_name(&ctx.cpu.mmu.cart().name(), &ctx.cpu);
        }
        if std::mem::take(&mut ctx.requests.load_state) {
            load_state_with_name(
                &ctx.cpu.mmu.cart().name(),
                &mut ctx.cpu,
            );
        }
        let (ticks_to_advance, ops_to_advance) = match ctx.run_state {
            RunState::Paused => {
                // When paused, sleep a little bit so we don't busy wait
                drop(ctx);
                std::thread::sleep(std::time::Duration::from_micros(500));
                continue;
            }
            RunState::StepTicks(ticks) => (ticks as usize, usize::MAX),
            RunState::StepOps(ops) => (usize::MAX, ops as usize),
            RunState::Running => (TICKS_TO_ADVANCE_PER_RUN_STEP, usize::MAX),
        };
        last_tick_time = std::time::Instant::now();
        let mut ticks = 0;
        let mut ops = 0;
        loop {
            if ctx.cpu.stopped()
                || ticks >= ticks_to_advance
                || ops >= ops_to_advance
            {
                break;
            }
            ticks += 1;
            if ctx.debug && debug::break_::check_and_reset() {
                crate::debug_log!("Hit breakpoint '{0:04X?}'", ctx.cpu.regs.pc);
                ctx.run_state = RunState::Paused;
                break;
            }
            let this_cycle: Tick = ctx.cpu.tick();
            if ctx.debug && this_cycle.breakpoint_was_hit {
                ctx.run_state = RunState::Paused;
                crate::debug_log!("Hit breakpoint '{0:04X?}'", this_cycle.pc);
                ctx.cpu.breakpoints.reset_count(this_cycle.pc);
                break;
            }
            if this_cycle.op_was_retired {
                ops += 1;
                ctx.last_op_address = Some(this_cycle.pc);
            }
        }
        match ctx.run_state {
            RunState::StepTicks(..) | RunState::StepOps(..) => {
                ctx.run_state = RunState::Paused
            }
            RunState::Running => {
                if last_save_time.elapsed() > std::time::Duration::from_secs(5)
                {
                    last_save_time = std::time::Instant::now();
                    let mut saved_state = Vec::new();
                    save_states::save_to_vec(&ctx.cpu, &mut saved_state);
                    ctx.saved_state = Some(saved_state.into_boxed_slice());
                }
                // Explicitly drop the mutex before sleeping, so we hold the
                // lock for as little time as possible.
                let speedup = ctx.speedup;
                drop(ctx);
                if speedup > 0.0 {
                    let expected_tick_time =
                        TICK_DURATION * (TICKS_TO_ADVANCE_PER_RUN_STEP as u32);
                    let actual_tick_time = last_tick_time.elapsed();
                    if expected_tick_time < actual_tick_time {
                        crate::debug_log!(
                            "executing ticks took {}µs longer \
                             than it was supposed to!",
                            (actual_tick_time - expected_tick_time).as_micros()
                        );
                    } else {
                        let time_to_sleep =
                            expected_tick_time - actual_tick_time;
                        let slowdown = 1.0 / speedup;
                        crate::utils::spin_sleep(
                            time_to_sleep.mul_f32(slowdown),
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

struct GuiClient<'a> {
    ctx: &'a Mutex<RunCtx>,
}

impl<'a> gui::Client for GuiClient<'a> {
    fn handle_event(&mut self, event: gui::Event) -> ControlFlow<()> {
        let mut ctx = self.ctx.lock();
        use crate::io::joypad::keys as joypad;
        use sdl2::keyboard::Keycode as K;

        fn sdl_key_to_joypad_code(key: &K) -> Option<u8> {
            Some(match key {
                K::Num1 => joypad::START,
                K::Num2 => joypad::SELECT,
                K::Z => joypad::B,
                K::X => joypad::A,
                K::Left => joypad::LEFT,
                K::Right => joypad::RIGHT,
                K::Up => joypad::UP,
                K::Down => joypad::DOWN,
                _ => return None,
            })
        }

        use gui::Event as E;
        match event {
            E::Exit | gui::Event::KeyDown(K::Q) => {
                return ControlFlow::Break(());
            }
            E::KeyDown(K::Tab) => {
                ctx.debug = !ctx.debug;
            }
            E::KeyDown(K::R) => {
                reset_cpu(&mut ctx.cpu);
                debug::log::reset();
            }
            E::KeyDown(K::T) => {
                ctx.run_state = RunState::StepTicks(1);
            }
            E::KeyDown(K::N) => {
                ctx.run_state = RunState::StepOps(1);
            }
            E::KeyDown(K::S) => {
                ctx.requests.save_state = true;
            }
            E::KeyDown(K::L) => {
                ctx.requests.load_state = true;
            }
            E::KeyDown(K::Space) => {
                if ctx.run_state == RunState::Running {
                    ctx.run_state = RunState::Paused;
                } else {
                    ctx.run_state = RunState::Running;
                }
            }
            E::KeyUp(K::Num3) => {
                ctx.cpu.mmu.io.joypad.up(joypad::A);
                ctx.cpu.mmu.io.joypad.up(joypad::B);
                ctx.cpu.mmu.io.joypad.up(joypad::START);
                ctx.cpu.mmu.io.joypad.up(joypad::SELECT);
            }
            E::KeyDown(K::Num3) => {
                ctx.cpu.mmu.io.joypad.down(joypad::A);
                ctx.cpu.mmu.io.joypad.down(joypad::B);
                ctx.cpu.mmu.io.joypad.down(joypad::START);
                ctx.cpu.mmu.io.joypad.down(joypad::SELECT);
            }
            E::KeyDown(K::Slash) => {
                if let Some(saved_state) = &ctx.saved_state {
                    ctx.emu.cpu = save_states::load_from_slice(saved_state);
                }
            }
            E::KeyDown(K::Equals) => {
                ctx.volumes[0] =
                    utils::partial_min(100.0, ctx.volumes[0] + 10.0);
            }
            E::KeyDown(K::Minus) => {
                ctx.volumes[0] = utils::partial_max(0.0, ctx.volumes[0] - 10.0);
            }
            E::KeyUp(key) => {
                if let Some(code) = sdl_key_to_joypad_code(&key) {
                    ctx.cpu.mmu.io.joypad.up(code);
                }
            }
            E::KeyDown(key) => {
                if let Some(code) = sdl_key_to_joypad_code(&key) {
                    ctx.cpu.mmu.io.joypad.down(code);
                }
            }
            E::Unknown(..) => {}
        };
        ControlFlow::Continue(())
    }

    fn draw_ui(&mut self, ui: &imgui::Ui, screen_texture_id: imgui::TextureId) {
        let mut ctx = self.ctx.lock();

        const X: usize = 0;
        const Y: usize = 1;
        fn sum(a: [f32; 2], b: [f32; 2]) -> [f32; 2] {
            [a[0] + b[0], a[1] + b[1]]
        }
        if ctx.debug {
            use crate::utils::Dump;
            let mut dump_str = String::with_capacity(0x1000);
            macro_rules! dump {
                ($e:expr) => {{
                    dump_str.clear();
                    ($e).dump(&mut dump_str).unwrap();
                    &dump_str
                }};
            }

            const DISPLAY_SCALE: f32 = 2.5;
            const DISPLAY_POS: [f32; 2] = [020.0, 000.0];
            const DISPLAY_SIZE: [f32; 2] =
                [DISPLAY_SCALE * 160.0, DISPLAY_SCALE * 144.0];
            ui.window("display")
                .size(DISPLAY_SIZE, imgui::Condition::Always)
                .position(DISPLAY_POS, imgui::Condition::Always)
                .movable(false)
                .build(|| {
                    ui.get_window_draw_list()
                        .add_image(
                            screen_texture_id,
                            sum(
                                ui.window_pos(),
                                ui.window_content_region_min(),
                            ),
                            sum(
                                ui.window_pos(),
                                ui.window_content_region_max(),
                            ),
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
            const VOLUME_POS: [f32; 2] =
                [JOYPAD_POS[X], JOYPAD_POS[Y] + JOYPAD_SIZE[Y]];
            const VOLUME_SIZE: [f32; 2] = [JOYPAD_SIZE[X], 200.0];
            ui.window("volume")
                .size(VOLUME_SIZE, imgui::Condition::FirstUseEver)
                .position(VOLUME_POS, imgui::Condition::FirstUseEver)
                .build(|| {
                    ui.slider("master", 0.0, 100.0, &mut ctx.volumes[0]);
                    ui.slider("channel 1", 0.0, 100.0, &mut ctx.volumes[1]);
                    ui.slider("channel 2", 0.0, 100.0, &mut ctx.volumes[2]);
                    ui.slider("channel 3", 0.0, 100.0, &mut ctx.volumes[3]);
                    ui.slider("channel 4", 0.0, 100.0, &mut ctx.volumes[4]);
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
                    ui.input_float("speedup", &mut ctx.speedup).build();
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
            const DISASM_POS: [f32; 2] =
                [REGS_POS[X] + REGS_SIZE[X], REGS_POS[Y]];
            const DISASM_SIZE: [f32; 2] = [200.0, 300.0];
            ui.window("disassembly")
                .size(DISASM_SIZE, imgui::Condition::FirstUseEver)
                .position(DISASM_POS, imgui::Condition::FirstUseEver)
                .movable(false)
                .build(|| {
                    let current_pc = ctx.cpu.regs.pc;
                    let last_pc =
                        if let Some(last_op_address) = ctx.last_op_address {
                            if current_pc > last_op_address
                                && current_pc - last_op_address <= 3
                            {
                                last_op_address
                            } else {
                                current_pc
                            }
                        } else {
                            current_pc
                        };
                    let bytes = ctx.cpu.mmu.block_load(last_pc, usize::MAX);
                    let insts: Vec<_> = disassembler::InstIter::new(bytes)
                        .map_while(|it| {
                            (it.offset <= current_pc as usize
                                || !opcodes::is_unconditional_jump(it.code()))
                            .then(|| {
                                let address = last_pc + (it.offset as u16);
                                format!(
                                    "{}{:04X}: {}",
                                    if last_pc + (it.offset as u16)
                                        == current_pc
                                    {
                                        "-->"
                                    } else {
                                        "   "
                                    },
                                    address,
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
            ui.window("apu")
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
                    if debug_log_was_written!()
                        && ui.scroll_y() == ui.scroll_max_y()
                    {
                        ui.set_scroll_here_y_with_ratio(1.0);
                    }
                })
                .unwrap();
        } else {
            const DISPLAY_SCALE: f32 = 4.0;
            const DISPLAY_POS: [f32; 2] = [020.0, 000.0];
            const DISPLAY_SIZE: [f32; 2] =
                [DISPLAY_SCALE * 160.0, DISPLAY_SCALE * 144.0];
            ui.window("display")
                .position(DISPLAY_POS, imgui::Condition::FirstUseEver)
                .size(DISPLAY_SIZE, imgui::Condition::Always)
                .focused(true)
                .movable(false)
                .mouse_inputs(false)
                .no_decoration()
                .build(|| {
                    if ui.is_mouse_hovering_rect(
                        ui.window_content_region_min(),
                        ui.window_content_region_max(),
                    ) {
                        ui.set_mouse_cursor(None);
                    }
                    ui.get_window_draw_list()
                        .add_image(
                            screen_texture_id,
                            sum(
                                ui.window_pos(),
                                ui.window_content_region_min(),
                            ),
                            sum(
                                ui.window_pos(),
                                ui.window_content_region_max(),
                            ),
                        )
                        .build();
                });
            const VOLUME_POS: [f32; 2] =
                [DISPLAY_POS[X] + DISPLAY_SIZE[X], DISPLAY_POS[Y]];
            const VOLUME_SIZE: [f32; 2] = [150.0, 100.0];
            ui.window("volume")
                .collapsible(false)
                .position(VOLUME_POS, imgui::Condition::FirstUseEver)
                .size(VOLUME_SIZE, imgui::Condition::Always)
                .build(|| {
                    ui.slider("vol", 0.0, 100.0, &mut ctx.volumes[0]);
                })
                .unwrap();
        }
    }

    fn render_frame(
        &mut self,
        screen: &mut gui::ScreenBuffer,
    ) -> crate::io::ppu::RenderUpdate {
        let ctx = self.ctx.lock();
        ctx.cpu.mmu.io.lcd.render(screen)
    }
}
