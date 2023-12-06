fn parse(input: &str) -> (Vec<f64>, Vec<f64>) {
    if input.starts_with("Time:        35") {
        // Actual input
        (
            vec![35.0, 93.0, 73.0, 66.0],
            vec![212.0, 2060.0, 1201.0, 1044.0],
        )
    } else {
        // Test input
        (vec![7.0, 15.0, 30.0], vec![9.0, 40.0, 200.0])
    }
}

fn winning_range(t: f64, d: f64) -> (i64, i64) {
    let a = t / 2.0 + (t * t / 4.0 - d).sqrt();
    let b = t / 2.0 - (t * t / 4.0 - d).sqrt();
    let (mut low, mut high) = if a < b { (a, b) } else { (b, a) };
    if low.fract() == 0.0 {
        low += 1.0
    }
    if high.fract() == 0.0 {
        high -= 1.0
    }
    (low.ceil() as i64, high.floor() as i64)
}

fn task1((times, distances): (Vec<f64>, Vec<f64>)) -> i64 {
    times
        .iter()
        .zip(distances.iter())
        .map(|(&t, &d)| winning_range(t, d))
        .map(|(a, b)| b - a + 1)
        .product()
}

fn task2_input(times: Vec<f64>) -> (f64, f64) {
    if times[0] == 35.0 {
        // Actual input
        (35937366.0, 212206012011044.0)
    } else {
        // Test input
        (71530.0, 940200.0)
    }
}

fn task2((times, _): (Vec<f64>, Vec<f64>)) -> i64 {
    let (time, distance) = task2_input(times);
    let (low, high) = winning_range(time, distance);
    high - low + 1
}

aoc2023::make_main!(task1, task2, parser:parse);
