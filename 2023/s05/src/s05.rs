use aoc2023::parse::{decimals, decimals_line};
use nom::{
    bytes::complete::tag,
    character::complete::{newline, none_of},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    IResult,
};
use std::ops;

#[derive(Clone, Debug, Default)]
struct Map {
    src_ranges: Vec<ops::Range<i64>>,
    dst_ranges: Vec<ops::Range<i64>>,
}

impl Map {
    fn apply(&self, src: i64) -> i64 {
        self.src_ranges
            .iter()
            .zip(self.dst_ranges.iter())
            .filter_map(|(src_range, dst_range)| {
                if src_range.contains(&src) {
                    Some(dst_range.start + src - src_range.start)
                } else {
                    None
                }
            })
            .next()
            .unwrap_or(src)
    }
}

fn parse_map(input: &str) -> IResult<&str, Map> {
    let (input, _) = terminated(many0(none_of(":")), tag(":\n"))(input)?;
    let (remainder, ranges) = many1(terminated(decimals_line, newline))(input)?;
    let map = ranges
        .iter()
        .filter(|range| !range.is_empty())
        .map(|range| {
            (
                ops::Range {
                    start: range[0],
                    end: range[0] + range[2],
                },
                ops::Range {
                    start: range[1],
                    end: range[1] + range[2],
                },
            )
        })
        .fold(Map::default(), |mut acc, (src, dst)| {
            acc.dst_ranges.push(src);
            acc.src_ranges.push(dst);
            acc
        });
    Ok((remainder, map))
}

fn parse(input: &str) -> IResult<&str, (Vec<i64>, Vec<Map>)> {
    let (input, seeds) = preceded(tag("seeds: "), decimals)(input)?;
    let (remainder, maps) = many1(parse_map)(input)?;
    Ok((remainder, (seeds, maps)))
}

fn find_location(seed: i64, maps: &[Map]) -> i64 {
    maps.iter().fold(seed, |x, map| map.apply(x))
}

fn task1((seeds, maps): (Vec<i64>, Vec<Map>)) -> i64 {
    seeds
        .iter()
        .map(|seed| find_location(*seed, &maps))
        .min()
        .unwrap()
}

// fn task2((seeds, map): (Vec<i64>, Map)) -> i64 {
//     dbg!(seeds);
//     dbg!(map);
//     0
// }

aoc2023::make_main!(task1, nom_parser:parse);
