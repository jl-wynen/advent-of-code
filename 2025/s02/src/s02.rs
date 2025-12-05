use pest::iterators::Pairs;
use std::ops::Range;

#[derive(pest_derive::Parser)]
#[grammar = "s02/src/grammar.pest"]
pub struct AOCParser;

fn parse(raw: Pairs<Rule>) -> Vec<Range<usize>> {
    raw.into_iter()
        .next()
        .unwrap()
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::range => {
                let mut inner = pair.into_inner();
                let start = inner.next().unwrap().as_str().parse().unwrap();
                let end: usize = inner.next().unwrap().as_str().parse().unwrap();
                Some(Range {
                    start,
                    end: end + 1, // +1 because Range is exclusive but input is inclusive
                })
            }
            _ => None,
        })
        .collect()
}

fn is_invalid_task_1(number: usize) -> bool {
    let n_digits = number.ilog10() + 1;
    if n_digits % 2 == 1 {
        return false;
    }
    let divisor = 10usize.pow(n_digits / 2);
    let first = number / divisor;
    let second = number % divisor;
    first == second
}

fn task1(input: Pairs<Rule>) -> usize {
    let parsed = parse(input);
    parsed
        .into_iter()
        .map(|range| {
            range
                .into_iter()
                .map(|number| if is_invalid_task_1(number) { number } else { 0 })
                .sum::<usize>()
        })
        .sum()
}

fn is_invalid_task_2(number: usize) -> bool {
    let n_digits = number.ilog10() + 1;
    for n_chunks in 2..n_digits + 1 {
        if n_digits % n_chunks != 0 {
            continue;
        }

        let mut chunks = Vec::with_capacity(n_chunks as usize);
        for _ in 0..n_chunks {
            chunks.clear();
            let divisor = 10usize.pow(n_digits / n_chunks);
            let mut num = number;
            while num > 0 {
                chunks.push(num % divisor);
                num /= divisor;
            }
        }
        if chunks.iter().all(|chunk| *chunk == chunks[0]) {
            return true;
        }
    }
    false
}

fn task2(input: Pairs<Rule>) -> usize {
    let parsed = parse(input);
    parsed
        .into_iter()
        .map(|range| {
            range
                .into_iter()
                .map(|number| if is_invalid_task_2(number) { number } else { 0 })
                .sum::<usize>()
        })
        .sum()
}

aoc2025::make_main!(task1, task2, parser: AOCParser, rule: Rule::ranges);
