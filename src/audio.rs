use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

pub enum Action {
    Play,
    Pause,
    Exit,
}

pub fn main_loop<I, E, D>(init: I, mut tick: E, mut data_callback: D)
where
    I: FnOnce(u32, u16),
    E: FnMut() -> Action,
    D: FnMut(&mut [f32]) + Send + 'static,
{
    let host = cpal::default_host();
    let device = host.default_output_device().unwrap();
    let desired_sample_rate =
        (crate::io::apu::MAIN_CLOCK_FREQUENCY / 16) as u32;
    let best_supported_mono_config = device
        .supported_output_configs()
        .unwrap()
        .filter(|cfg| {
            cfg.channels() <= 2
                && cfg.min_sample_rate().0 < desired_sample_rate
                && desired_sample_rate <= cfg.max_sample_rate().0
                && cfg.sample_format() == cpal::SampleFormat::F32
        })
        .max_by_key(|cfg| (cfg.channels(), cfg.max_sample_rate()))
        .expect(
            "couldn't find suitable audio device! \
                (mono/stereo @ {desired_sample_rate} Hz)",
        )
        .with_sample_rate(cpal::SampleRate(desired_sample_rate));
    let sample_rate = best_supported_mono_config.sample_rate().0;
    let stream_config = best_supported_mono_config.config();
    init(sample_rate, stream_config.channels);
    let stream = device
        .build_output_stream(
            &stream_config,
            move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                data_callback(data);
            },
            |err| {
                crate::debug_log!("audio err: {err}");
            },
            None,
        )
        .unwrap();
    stream.play().unwrap();
    loop {
        match tick() {
            Action::Play => stream.play().unwrap(),
            Action::Pause => stream.pause().unwrap(),
            Action::Exit => break,
        };
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
