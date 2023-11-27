#[macro_export]
macro_rules! make_test_cases {
    (@step $idx:expr,) => {};
    (@step $idx:expr, $func_name:ident, $($tail:ident,)*) => {
        paste! {
                #[test]
                fn [<test_ $func_name>]() {
                    let input = fileio::read_test_input(file!());
                    let expected_output = fileio::read_test_output(file!(), $idx);
                    let actual_output = $func_name(input).trim().to_string();
                    assert_eq!(actual_output, expected_output);
                }
        }
        make_test_cases!(@step ($idx + 1), $($tail,)*);
    };
}

#[macro_export]
macro_rules! make_tests {
    ($($task:ident),+) => {
        #[cfg(test)]
        mod __aoc2023_auto_tests {
            use super::*;
            use paste::paste;
            use aoc2023::make_test_cases;

            make_test_cases!(@step 1, $($task,)*);
        }
    };
}

#[macro_export]
macro_rules! make_main {
    ($task1:ident) => {
        fn main() {
            use aoc2023::fileio;
            let input = fileio::read_input(file!());
            let res = $task1(input);
            println!("!! Task 1 result: <|{res}|>");
        }

        aoc2023::make_tests!($task1);
    };
    ($task1:ident, $task2:ident) => {
        fn main() {
            use aoc2023::fileio;
            let input = fileio::read_input(file!());
            let res1 = $task1(input.clone());
            println!("!! Task 1 result: <|{res1}|>");
            let res2 = $task2(input);
            println!("!! Task 2 result: <|{res2}|>");
        }

        aoc2023::make_tests!($task1, $task2);
    };
}
