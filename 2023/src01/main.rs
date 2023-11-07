use std::fs;
use std::path::PathBuf;

use aoc2023;

fn source_dir() -> PathBuf {
    PathBuf::from(file!())
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

fn input_data_dir() -> PathBuf {
    source_dir().join("inputs")
}

fn output_data_dir() -> PathBuf {
    source_dir().join("outputs")
}

fn read_test_input() -> String {
    fs::read_to_string(input_data_dir().join("test-input")).unwrap()
}

fn read_test_output() -> String {
    fs::read_to_string(output_data_dir().join("expected-output")).unwrap()
}

fn part1_solution(input: &str) -> String {
    println!("Part1 input: {}", input);
    "solution1".to_string()
}

aoc2023::init!(part1_solution);

// fn main() {
// println!("Input: {}", read_test_input());
// println!("Output: {}", read_test_output());
// println!("p = {}", aoc2023::P);
// }
