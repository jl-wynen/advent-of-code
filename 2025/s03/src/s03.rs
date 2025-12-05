use pest::iterators::Pairs;

#[derive(pest_derive::Parser)]
#[grammar = "s03/src/grammar.pest"]
pub struct AOCParser;

fn parse(raw: Pairs<Rule>) -> Vec<Vec<u8>> {
    raw.into_iter()
        .next()
        .unwrap()
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::bank => Some(
                pair.into_inner()
                    .map(|p| p.as_str().parse().unwrap())
                    .collect(),
            ),
            _ => None,
        })
        .collect()
}

fn max_in_bank(bank: &[u8], n_digits: usize) -> i64 {
    let (max_index, digit) = bank[..bank.len() - n_digits + 1].iter().enumerate().fold(
        (0, 0),
        |acc, (index, joltage)| {
            if joltage > &acc.1 {
                (index, *joltage)
            } else {
                acc
            }
        },
    );
    if n_digits > 1 {
        let remainder = max_in_bank(&bank[max_index + 1..], n_digits - 1);
        let res = (digit as i64) * 10i64.pow(n_digits as u32 - 1) + remainder;
        res
    } else {
        digit as i64
    }
}

fn task1(input: Pairs<Rule>) -> i64 {
    let parsed = parse(input);
    parsed.iter().map(|bank| max_in_bank(bank, 2)).sum()
}

fn task2(input: Pairs<Rule>) -> i64 {
    let parsed = parse(input);
    parsed.iter().map(|bank| max_in_bank(bank, 12)).sum()
}

aoc2025::make_main!(task1, task2, parser: AOCParser, rule: Rule::banks);
