use pest::iterators::Pairs;
use std::ops::RangeInclusive;

#[derive(pest_derive::Parser)]
#[grammar = "s05/src/grammar.pest"]
pub struct AOCParser;

fn parse(raw: Pairs<Rule>) -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let mut inventory = raw.into_iter().next().unwrap().into_inner();
    let fresh_ranges = inventory.next().unwrap().into_inner();
    let available = inventory.next().unwrap().into_inner();
    (parse_ranges(fresh_ranges), parse_ids(available))
}

fn parse_ranges(ranges: Pairs<Rule>) -> Vec<RangeInclusive<usize>> {
    ranges
        .into_iter()
        .map(|r| {
            let mut inner = r.into_inner();
            let start = inner.next().unwrap().as_str().parse::<usize>().unwrap();
            let end = inner.next().unwrap().as_str().parse::<usize>().unwrap();
            start..=end
        })
        .collect()
}

fn parse_ids(ids: Pairs<Rule>) -> Vec<usize> {
    ids.into_iter()
        .map(|id| id.as_str().parse::<usize>().unwrap())
        .collect()
}

fn merge_overlapping(mut ranges: Vec<RangeInclusive<usize>>) -> Vec<RangeInclusive<usize>> {
    ranges.sort_by(|a, b| a.start().cmp(b.start()));
    let mut merged = Vec::with_capacity(ranges.len());
    merged.push(ranges[0].clone());
    for range in ranges[1..].into_iter() {
        let last = merged.last().unwrap().clone();
        if range.contains(last.end()) {
            merged.pop();
            let &new_end = range.end().max(last.end());
            merged.push(*last.start()..=new_end);
        } else {
            merged.push(range.clone());
        }
    }
    merged
}

fn task1(input: Pairs<Rule>) -> i64 {
    let (fresh_ranges, available) = parse(input);
    let fresh_ranges = merge_overlapping(fresh_ranges);
    available
        .iter()
        .map(|id| {
            for range in &fresh_ranges {
                if range.contains(id) {
                    return 1;
                }
            }
            return 0;
        })
        .sum()
}

fn task2(input: Pairs<Rule>) -> usize {
    let (fresh_ranges, _) = parse(input);
    let fresh_ranges = merge_overlapping(fresh_ranges);
    fresh_ranges
        .iter()
        .map(|range| range.end() - range.start() + 1)
        .sum()
}

aoc2025::make_main!(task1,task2, parser: AOCParser, rule: Rule::inventory);
