use pest::iterators::Pairs;

#[derive(pest_derive::Parser)]
#[grammar = "s01/src/grammar.pest"]
pub struct AOCParser;

#[derive(Debug, Clone, Copy)]
enum Turn {
    Left(i64),
    Right(i64),
}

fn parse(raw: Pairs<Rule>) -> Vec<Turn> {
    raw.into_iter()
        .next()
        .unwrap()
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::turn => {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::left => Some(Turn::Left(inner.as_str()[1..].parse().unwrap())),
                    Rule::right => Some(Turn::Right(inner.as_str()[1..].parse().unwrap())),
                    _ => None,
                }
            }
            _ => None,
        })
        .collect()
}

fn task1(input: Pairs<Rule>) -> i64 {
    let turns = parse(input);
    let mut zeros = 0;
    let mut pos = 50;
    for turn in turns {
        match turn {
            Turn::Left(n) => pos = (pos - n) % 100,
            Turn::Right(n) => pos = (pos + n) % 100,
        }
        if pos == 0 {
            zeros += 1;
        }
    }
    zeros
}

fn task2(input: Pairs<Rule>) -> i64 {
    let turns = parse(input);
    let mut zeros = 0;
    let mut pos = 50;
    for turn in turns {
        let new_pos = match turn {
            Turn::Left(n) => {
                let new_pos = pos - n;
                if pos != 0 {
                    // was already counted
                    zeros += (100 - pos + n) / 100;
                }
                new_pos
            }
            Turn::Right(n) => {
                let new_pos = pos + n;
                zeros += new_pos / 100;
                new_pos
            }
        };
        pos = new_pos % 100;
        if pos < 0 {
            pos += 100;
        }
        println!("{turn:?}: {pos} ({new_pos}) -- {zeros}");
    }
    zeros
}

aoc2025::make_main!(task1, task2, parser: AOCParser, rule: Rule::combination);
