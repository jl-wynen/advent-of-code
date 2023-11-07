use std::fs;
use std::path::PathBuf;

fn source_dir(exe_file: &str) -> PathBuf {
    PathBuf::from(exe_file)
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn input_data_dir(exe_file: &str) -> PathBuf {
    source_dir(exe_file).join("inputs")
}

fn output_data_dir(exe_file: &str) -> PathBuf {
    source_dir(exe_file).join("outputs")
}

pub fn read_test_input(exe_file: &str) -> String {
    fs::read_to_string(input_data_dir(exe_file).join("test-input")).unwrap()
}

pub fn read_test_output(exe_file: &str, part_no: u8) -> String {
    fs::read_to_string(output_data_dir(exe_file).join(format!("expected-output-{part_no}")))
        .unwrap()
}

pub fn read_input(exe_file: &str) -> String {
    fs::read_to_string(input_data_dir(exe_file).join("input")).unwrap()
}
