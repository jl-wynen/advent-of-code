#[macro_export]
macro_rules! make_test_cases {
    (step: $idx:expr,, parser: $parser:expr) => {};
    (step: $idx:expr, $func_name:ident, $($tail:ident,)*, parser: $parser:expr) => {
        paste! {
                #[test]
                fn [<test_ $func_name>]() {
                    use aoc2023::fileio;

                    let input = fileio::read_test_input(file!(), $idx);
                    let parsed = $parser(input.as_str());
                    let expected_output = fileio::read_test_output(file!(), $idx);
                    let actual_output = $func_name(parsed).to_string();
                    assert_eq!(actual_output, expected_output);
                }
        }
        make_test_cases!(step: ($idx + 1), $($tail,)*, parser: $parser);
    };
}

#[macro_export]
macro_rules! make_tests {
    ($parser:expr, $($task:ident),+) => {
        #[cfg(test)]
        mod __aoc2023_auto_tests {
            use super::*;
            use paste::paste;
            use aoc2023::make_test_cases;

            make_test_cases!(step: 1, $($task,)*, parser: $parser);
        }
    };
}

#[macro_export]
macro_rules! wrap_parser {
    ($nom_parser:expr) => {
        |input: &str| {
            use aoc2023::parse;
            parse::apply($nom_parser, input)
        }
    };
}

#[macro_export]
macro_rules! make_main {
    ($task1:ident, parser: $parser:expr) => {
        fn main() {
            use aoc2023::fileio;

            let input = fileio::read_input(file!());
            let parsed = $parser(input.as_str());
            let res = $task1(parsed);
            println!("!! Task 1 result: >{res}<");
        }

        aoc2023::make_tests!($parser, $task1);
    };
    ($task1:ident, $task2:ident, parser: $parser:expr) => {
        fn main() {
            use aoc2023::fileio;

            let input = fileio::read_input(file!());
            let parsed = $parser(input.as_str());
            let res1 = $task1(parsed.clone());
            println!("!! Task 1 result: >{res1}<");
            let res2 = $task2(parsed);
            println!("!! Task 2 result: >{res2}<");
        }

        aoc2023::make_tests!($parser, $task1, $task2);
    };
    ($task1:ident, nom_parser: $parser:expr) => {
        aoc2023::make_main!($task1, parser: aoc2023::wrap_parser!($parser));
    };
    ($task1:ident, $task2:ident, nom_parser: $parser:expr) => {
        aoc2023::make_main!($task1, $task2, parser: aoc2023::wrap_parser!($parser));
    };
    ($task1:ident) => {
        aoc2023::make_main!($task1, parser: std::convert::identity);
    };
    ($task1:ident, $task2:ident) => {
        aoc2023::make_main!($task1, $task2, parser: std::convert::identity);
    };
}
