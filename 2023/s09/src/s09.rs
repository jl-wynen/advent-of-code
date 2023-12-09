fn parse(input: &str) -> Vec<Vec<i64>> {
    input
        .lines()
        .map(|line| line.split(' ').map(|x| x.parse().unwrap()).collect())
        .collect()
}

fn extrapolate_forward(mut history: Vec<i64>) -> i64 {
    let mut current_len = history.len();
    while !&history[0..current_len - 1].iter().all(|&d| d == 0) {
        for i in 0..current_len - 1 {
            history[i] = history[i + 1] - history[i];
        }
        current_len -= 1;
    }
    history.iter().sum()
}

fn task1(histories: Vec<Vec<i64>>) -> i64 {
    histories.into_iter().map(extrapolate_forward).sum()
}

fn extrapolate_backward(mut history: Vec<i64>) -> i64 {
    let mut current_start = 0;
    while !&history[current_start..history.len() - 1]
        .iter()
        .all(|&d| d == 0)
    {
        for i in (current_start..history.len() - 1).rev() {
            history[i + 1] -= history[i];
        }
        current_start += 1;
    }
    history.iter().rev().copied().reduce(|a, b| b - a).unwrap()
}

fn task2(histories: Vec<Vec<i64>>) -> i64 {
    histories.into_iter().map(extrapolate_backward).sum()
}

aoc2023::make_main!(task1, task2, parser:parse);
