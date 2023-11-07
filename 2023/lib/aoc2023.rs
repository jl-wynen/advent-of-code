pub mod fileio;

#[macro_export]
macro_rules! make_part1 {
    ($func: ident) => {
        fn aoc2023_part1_wrapper(input: &str) {
            let output = $func(&input);
            println!("Part1: >|{}|<", output);
        }
    };
}

#[macro_export]
macro_rules! make_part2 {
    ($func: ident) => {
        fn aoc2023_part2_wrapper(input: &str) {
            let output = $func(&input);
            println!("Part2: >|{}|<", output);
        }
    };
    () => {
        fn aoc2023_part2_wrapper(input: &str) {}
    };
}

#[macro_export]
macro_rules! make_main {
    ($part1: ident) => {
        use aoc2023;
        aoc2023::make_part1!($part1);

        fn main() {
            use aoc2023::fileio::read_input;

            let input = read_input(file!());
            aoc2023_part1_wrapper(&input);
        }
    };
    ($part1: ident, $part2: ident) => {
        use aoc2023;
        aoc2023::make_part1!($part1);
        aoc2023::make_part2!($part2);

        fn main() {
            use aoc2023::fileio::read_input;

            let input = read_input(file!());
            aoc2023_part1_wrapper(&input);
            aoc2023_part2_wrapper(&input);
        }
    };
}

#[macro_export]
macro_rules! init {
    ($part1: ident) => {
        use aoc2023;
        aoc2023::make_main!($part1);
    };
    ($part1: ident, $part2: ident) => {
        use aoc2023;
        aoc2023::make_main!($part1, $part2);
    };
}
