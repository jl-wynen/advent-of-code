#[macro_export]
macro_rules! make_tests {
    ($task1:ident) => {
        #[cfg(test)]
        mod __aoc2023_auto_tests {
            use super::*;

            #[test]
            fn test_task1() {
                let input = fileio::read_test_input(file!());
                let expected_output = fileio::read_test_output(file!(), 1);
                let actual_output = $task1(input).trim().to_string();
                assert_eq!(actual_output, expected_output);
            }
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

        // aoc2023::make_tests!($task1, $task2);
    };
}
