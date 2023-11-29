use aoc2023::parse::decimals;

fn task1(input: Vec<i64>) -> String {
    input.iter().sum::<i64>().to_string()
}

fn task2(input: Vec<i64>) -> String {
    input.iter().product::<i64>().to_string()
}

aoc2023::make_main!(task1, task2, parser: decimals);
