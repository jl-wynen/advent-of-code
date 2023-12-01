fn find_digits(s: &String) -> (u32, u32) {
    let mut it = s.chars().filter_map(|c| c.to_digit(10));
    let first = it.next().unwrap();
    let second = it.fold(first, |_, c| c);
    (first, second)
}

fn glue_digits((first, second): (u32, u32)) -> u32 {
    first * 10 + second
}

fn task1(input: Vec<String>) -> String {
    input
        .iter()
        .map(find_digits)
        .map(glue_digits)
        .sum::<u32>()
        .to_string()
}

// fn task2(input: Vec<i64>) -> String {
//     input.iter().product::<i64>().to_string()
// }

fn parser(input: &str) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

aoc2023::make_main!(task1, parser: parser);
