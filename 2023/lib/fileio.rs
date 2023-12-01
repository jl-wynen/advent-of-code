use std::fs;
use std::path::PathBuf;

fn normalize_exe_path(exe_file: &str) -> PathBuf {
    let exe_path = PathBuf::from(exe_file);
    if !exe_path.exists() {
        PathBuf::from_iter(exe_path.components().skip(1))
    } else {
        exe_path
    }
}

fn source_dir(exe_file: &str) -> PathBuf {
    normalize_exe_path(exe_file)
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn input_data_dir(exe_file: &str) -> PathBuf {
    source_dir(exe_file).join("../inputs")
}

fn output_data_dir(exe_file: &str) -> PathBuf {
    source_dir(exe_file).join("../outputs")
}

pub fn read_test_input(exe_file: &str, part_no: u8) -> String {
    let path = input_data_dir(exe_file).join(format!("test-input-{part_no}"));
    let path = if path.exists() {
        path
    } else {
        input_data_dir(exe_file).join("test-input-1")
    };
    fs::read_to_string(path).unwrap()
}

pub fn read_test_output(exe_file: &str, part_no: u8) -> String {
    fs::read_to_string(output_data_dir(exe_file).join(format!("expected-test-output-{part_no}")))
        .unwrap()
        .trim()
        .into()
}

pub fn read_input(exe_file: &str) -> String {
    fs::read_to_string(input_data_dir(exe_file).join("input")).unwrap()
}
