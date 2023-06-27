use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

pub enum Action {
    Play,
    Pause,
    Exit,
}

pub fn loop_<I, E, D>(mut init: I, mut each_tick: E, mut data_callback: D)
where
    I: FnMut(u32),
    E: FnMut() -> Action,
    D: FnMut(&mut [f32]) -> () + Send + 'static,
{
    let host = cpal::default_host();
    let device = host.default_output_device().unwrap();
    let desired_sample_rate = (crate::io::apu::MAIN_CLOCK_FREQUENCY / 4) as u32;
    let best_supported_mono_config = device
        .supported_output_configs()
        .unwrap()
        .filter(|cfg| {
            cfg.channels() == 2
                && cfg.min_sample_rate().0 < desired_sample_rate
                && desired_sample_rate <= cfg.max_sample_rate().0
                && cfg.sample_format() == cpal::SampleFormat::F32
        })
        .max_by(|a, b| a.max_sample_rate().cmp(&b.max_sample_rate()))
        .unwrap()
        .with_sample_rate(cpal::SampleRate(desired_sample_rate));
    let sample_rate = best_supported_mono_config.sample_rate().0;
    let stream_config = best_supported_mono_config.config();
    init(sample_rate);
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
        match each_tick() {
            Action::Play => stream.play().unwrap(),
            Action::Pause => stream.pause().unwrap(),
            Action::Exit => break,
        };
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
