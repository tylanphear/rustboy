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
    Key(sdl2::keyboard::Keycode),
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
            } => Self::Key(code),
            _ => Self::Unknown(value),
        }
    }
}

pub fn main_loop<H, E, R>(
    mut handle_event: H,
    mut draw_ui: E,
    mut render_frame: R,
) where
    H: FnMut(Event) -> ControlFlow<()>,
    E: FnMut(&imgui::Ui, imgui::TextureId),
    R: FnMut(&mut crate::io::lcd::ScreenBuffer),
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
            RGB,
            UNSIGNED_BYTE,
            None,
        );
        renderer.texture_map_mut().register(id).unwrap()
    };

    let mut raw_frame_pixels = Box::new([0u8; 160 * 144]);
    let mut gl_frame_pixels = vec![0; 160 * 144 * 3];
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

        render_frame(raw_frame_pixels.as_mut());
        for (idx, raw_pixel) in raw_frame_pixels.iter().enumerate() {
            let gl_pixel = match raw_pixel {
                //     R     G     B
                0 => [0xFC, 0xFC, 0xFC], // WHITE
                1 => [0xD3, 0xD3, 0xD3], // LIGHT GRAY
                2 => [0x5A, 0x5A, 0x5A], // DARK GRAY
                3 => [0x00, 0x00, 0x00], // BLACK
                _ => unreachable!(),
            };
            gl_frame_pixels[3 * idx + 0] = gl_pixel[0];
            gl_frame_pixels[3 * idx + 1] = gl_pixel[1];
            gl_frame_pixels[3 * idx + 2] = gl_pixel[2];
        }
        unsafe {
            use glow::*;
            let gl = renderer.gl_context();
            let frame = renderer
                .texture_map()
                .gl_texture(framebuffer_tex_id)
                .unwrap();
            gl.bind_texture(TEXTURE_2D, Some(frame));
            gl.tex_image_2d(
                TEXTURE_2D,
                0,
                RGBA as i32,
                160,
                144,
                0,
                RGB,
                UNSIGNED_BYTE,
                Some(&gl_frame_pixels),
            );
            gl.bind_texture(TEXTURE_2D, None);
        }

        let draw_data = imgui.render();
        unsafe { renderer.gl_context().clear(glow::COLOR_BUFFER_BIT) };
        renderer.render(draw_data).unwrap();

        window.gl_swap_window();
    }
}
