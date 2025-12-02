#[macro_export]
macro_rules! make_test_cases {
    (step: $idx:expr,, parser: $parser:ty, rule: $rule:path) => {};
    (step: $idx:expr, $func_name:ident, $($tail:ident,)*, parser: $parser:ty, rule: $rule:path) => {
        paste! {
                #[test]
                fn [<test_ $func_name>]() {
                    use pest::Parser;
                    use aoc2025::fileio;

                    let input = fileio::read_test_input(file!(), $idx);
                    let parsed = <$parser>::parse($rule, input.as_str()).unwrap();
                    let expected_output = fileio::read_test_output(file!(), $idx);
                    let actual_output = $func_name(parsed).to_string();
                    assert_eq!(actual_output, expected_output);
                }
        }
        make_test_cases!(step: ($idx + 1), $($tail,)*, parser: $parser, rule: $rule);
    };
}

#[macro_export]
macro_rules! make_tests {
    ($parser:ty, $rule:path, $($task:ident),+) => {
        #[cfg(test)]
        mod __aoc2025_auto_tests {
            use super::*;
            use paste::paste;
            use aoc2025::make_test_cases;

            make_test_cases!(step: 1, $($task,)*, parser: $parser, rule: $rule);
        }
    };
}

#[macro_export]
macro_rules! make_main {
    ($task1:ident, parser: $parser:ty, rule: $rule:path) => {
        fn main() {
            use pest::Parser;
            use aoc2025::fileio;

            let input = fileio::read_input(file!());
            let parsed = <$parser>::parse($rule, input.as_str()).unwrap();
            let res = $task1(parsed);
            println!("!! Task 1 result: >{res}<");
        }

        aoc2025::make_tests!($parser, $rule, $task1);
    };
    ($task1:ident, $task2:ident, parser: $parser:ty, rule: $rule:path) => {
        fn main() {
            use pest::Parser;
            use aoc2025::fileio;

            let input = fileio::read_input(file!());
            let parsed = <$parser>::parse($rule, input.as_str()).unwrap();
            let res1 = $task1(parsed.clone());
            println!("!! Task 1 result: >{res1}<");
            let res2 = $task2(parsed);
            println!("!! Task 2 result: >{res2}<");
        }

        aoc2025::make_tests!($parser, $rule, $task1, $task2);
    };
}
