use pest::iterators::Pairs;

#[derive(pest_derive::Parser)]
#[grammar = "s01/src/grammar.pest"]
pub struct AOCParser;

// fn parse(raw: Pairs<Rule>) -> ! {
//     raw.into_iter()
//         .next()
//         .unwrap()
//         .into_inner()
//         .filter_map(|pair| match pair.as_rule() {
//
//         })
//         .collect()
// }

fn task1(input: Pairs<Rule>) -> i64 {
    // let parsed = parse(input);
    0
}

// fn task2(input: Pairs<Rule>) -> i64 {
//     0
// }

aoc2025::make_main!(task1, parser: AOCParser, rule: Rule::combination);
