use std::ops::ControlFlow;

use glow::HasContext;
use imgui::Context;
use imgui_glow_renderer::{AutoRenderer, TextureMap};
use imgui_sdl2_support::SdlPlatform;
use sdl2::video::{GLProfile, Window};

fn glow_context(window: &Window) -> glow::Context {
    unsafe {
        glow::Context::from_loader_function(|s| {
            window.subsystem().gl_get_proc_address(s) as _
        })
    }
}

pub enum Event {
    Exit,
    KeyDown(sdl2::keyboard::Keycode),
    KeyUp(sdl2::keyboard::Keycode),
    Unknown(sdl2::event::Event),
}

impl From<sdl2::event::Event> for Event {
    fn from(value: sdl2::event::Event) -> Self {
        use sdl2::event::Event as E;
        use sdl2::event::WindowEvent as WE;
        match value {
            E::Window {
                win_event: WE::Close,
                ..
            }
            | E::Quit { .. } => Self::Exit,
            E::KeyDown {
                keycode: Some(code),
                ..
            } => Self::KeyDown(code),
            E::KeyUp {
                keycode: Some(code),
                ..
            } => Self::KeyUp(code),
            _ => Self::Unknown(value),
        }
    }
}

pub struct ScreenBuffer {
    gl_pixels: Box<[u8; 160 * 144 * 4]>,
}

impl ScreenBuffer {
    pub fn new() -> Self {
        Self {
            gl_pixels: Box::new([0; 160 * 144 * 4]),
        }
    }
}

impl crate::ppu::ExternalScreenBuffer for ScreenBuffer {
    const NUM_RAW_PIXELS: usize = 4;

    #[inline]
    #[rustfmt::skip]
    fn write_pixel(&mut self, idx: usize, raw_pixel: u8) {
        use crate::ppu::colors;
        let luminosity = match raw_pixel & 0x3 {
            colors::WHITE => 0xFC,
            colors::LGRAY => 0xD3,
            colors::DGRAY => 0x5A,
            colors::BLACK => 0x00,
            _ => unreachable!(),
        };
        /* R */ self.gl_pixels[4 * idx + 0] = luminosity;
        /* G */ self.gl_pixels[4 * idx + 1] = luminosity;
        /* B */ self.gl_pixels[4 * idx + 2] = luminosity;
        /* A */ self.gl_pixels[4 * idx + 3] = 0xFF;
    }

    #[inline]
    fn slice(&self, start: usize, end: usize) -> &[u8] {
        &self.gl_pixels.as_slice()
            [start * Self::NUM_RAW_PIXELS..end * Self::NUM_RAW_PIXELS]
    }
}

pub fn main_loop<H, E, R>(
    mut handle_event: H,
    mut draw_ui: E,
    mut render_frame: R,
) where
    H: FnMut(Event) -> ControlFlow<()>,
    E: FnMut(&imgui::Ui, imgui::TextureId),
    R: FnMut(&mut ScreenBuffer) -> crate::io::ppu::RenderUpdate,
{
    let sdl = sdl2::init().expect("couldn't initialize SDL?");
    let video = sdl.video().expect("couldn't get SDL video subsystem?");
    video.gl_attr().set_context_version(3, 3);
    video.gl_attr().set_context_profile(GLProfile::Core);
    let window = video
        .window("rustyboy", 1280, 720)
        .allow_highdpi()
        .opengl()
        .position_centered()
        .resizable()
        .build()
        .expect("couldn't create window?");

    let gl_ctx = window
        .gl_create_context()
        .expect("couldn't get GL context?");
    window
        .gl_make_current(&gl_ctx)
        .expect("err setting GL context");
    window.subsystem().gl_set_swap_interval(1).unwrap();

    let glow_ctx = glow_context(&window);
    let mut imgui = Context::create();
    imgui.set_ini_filename(None);
    imgui.set_log_filename(None);
    imgui
        .fonts()
        .add_font(&[imgui::FontSource::DefaultFontData { config: None }]);

    let mut platform = SdlPlatform::init(&mut imgui);
    let mut renderer = AutoRenderer::initialize(glow_ctx, &mut imgui).unwrap();

    let mut event_pump = sdl.event_pump().unwrap();
    let framebuffer_tex_id = unsafe {
        use glow::*;
        let gl = renderer.gl_context();
        let id = gl.create_texture().unwrap();
        gl.bind_texture(TEXTURE_2D, Some(id));
        gl.tex_parameter_i32(TEXTURE_2D, TEXTURE_WRAP_S, REPEAT as i32);
        gl.tex_parameter_i32(TEXTURE_2D, TEXTURE_WRAP_T, REPEAT as i32);
        gl.tex_parameter_i32(TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST as i32);
        gl.tex_parameter_i32(TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST as i32);
        gl.tex_image_2d(
            TEXTURE_2D,
            0,
            RGB as i32,
            160,
            144,
            0,
            RGBA,
            UNSIGNED_BYTE,
            None,
        );
        gl.bind_texture(TEXTURE_2D, None);
        renderer.texture_map_mut().register(id).unwrap()
    };

    let mut screen_buffer = ScreenBuffer::new();
    'main: loop {
        for event in event_pump.poll_iter() {
            platform.handle_event(&mut imgui, &event);

            match handle_event(Event::from(event)) {
                ControlFlow::Break(()) => break 'main,
                ControlFlow::Continue(()) => continue,
            }
        }

        platform.prepare_frame(&mut imgui, &window, &event_pump);

        let ui = imgui.new_frame();
        draw_ui(ui, framebuffer_tex_id);

        let update = render_frame(&mut screen_buffer);
        if update.num_scanlines() > 0 {
            unsafe {
                use glow::*;
                let gl = renderer.gl_context();
                let frame = renderer
                    .texture_map()
                    .gl_texture(framebuffer_tex_id)
                    .unwrap();
                gl.bind_texture(TEXTURE_2D, Some(frame));
                gl.tex_sub_image_2d(
                    TEXTURE_2D,
                    0,
                    0,
                    update.start_scanline as i32,
                    crate::io::ppu::SCREEN_WIDTH as i32,
                    update.num_scanlines() as i32,
                    RGBA,
                    UNSIGNED_BYTE,
                    PixelUnpackData::Slice(update.section(&screen_buffer)),
                );
                //gl.tex_image_2d(
                //    TEXTURE_2D,
                //    0,
                //    RGBA as i32,
                //    160,
                //    144,
                //    0,
                //    RGBA,
                //    UNSIGNED_BYTE,
                //    Some(screen_buffer.gl_pixels.as_slice()),
                //);
                gl.bind_texture(TEXTURE_2D, None);
            }
        }

        let draw_data = imgui.render();
        unsafe { renderer.gl_context().clear(glow::COLOR_BUFFER_BIT) };
        renderer.render(draw_data).unwrap();
        window.gl_swap_window();
    }
}
