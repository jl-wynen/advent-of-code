use aoc2023::parse::ws;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, multispace1, newline, one_of},
    combinator::map_res,
    multi::many1,
    sequence::{delimited, separated_pair, terminated},
    IResult,
};

use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
enum Dir {
    L,
    R,
}
type Map = HashMap<u32, (u32, u32)>;

fn parse_node(input: &str) -> IResult<&str, u32> {
    let (input, chars) = alpha1(input)?;
    let code = chars.bytes().fold(0, |acc, c| {
        dbg!(c as u32);
        acc * 256 + c as u32
    });
    Ok((input, code))
}

fn parse_map(input: &str) -> IResult<&str, Map> {
    let (input, elems) = many1(terminated(
        separated_pair(
            parse_node,
            ws(tag("=")),
            delimited(
                tag("("),
                separated_pair(parse_node, tag(", "), parse_node),
                tag(")"),
            ),
        ),
        newline,
    ))(input)?;
    let map = Map::from_iter(elems.iter().copied());
    Ok((input, map))
}

fn parse(input: &str) -> IResult<&str, (Vec<Dir>, Map)> {
    let (input, directions) = many1(one_of("LR"))(input)?;
    let directions = directions
        .iter()
        .map(|d| if *d == 'R' { Dir::R } else { Dir::L })
        .collect();
    let (input, _) = multispace1(input)?;

    let (input, map) = parse_map(input)?;

    Ok((input, (directions, map)))
}

fn task1((dir, map): (Vec<Dir>, Map)) -> i64 {
    dbg!(dir);
    dbg!(map);
    0
}

// fn task2(input: &str) -> i64 {
//     input.to_string()
// }

aoc2023::make_main!(task1, nom_parser:parse);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_task1_alt() {
        //expected: 6
    }

    #[test]
    fn test_parse_node() {
        let (r, n) = parse_node("AAA").unwrap();
        assert_eq!(r, "");
        assert_eq!(n, 97 * 256 * 256 + 97 * 256 + 97);
        let (r, n) = parse_node("BBB").unwrap();
        assert_eq!(r, "");
        assert_eq!(n, 98 * 256 * 256 + 98 * 256 + 98);
    }
}
