fn task1(input: usize) -> String {
    println!("task1: {input}");
    "EXPECTED OUTPUT 1".into()
}

fn task2(input: usize) -> String {
    println!("task1: {input}");
    "EXPECTED OUTPUT 2".into()
}

fn parse(input: String) -> usize {
    input.len()
}

aoc2023::make_main!(task1, task2, parser: parse);
