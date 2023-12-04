use aoc2023::parse::{decimal, decimals, ws};
use nom::{
    bytes::complete::tag,
    multi::many1,
    sequence::{delimited, separated_pair},
    IResult,
};

#[derive(Debug)]
struct Card {
    id: i64,
    winning: Vec<i64>,
    have: Vec<i64>,
}

fn parse_card(input: &str) -> IResult<&str, Card> {
    let (input, id) = ws(decimal)(input)?;
    let (input, _) = ws(tag(":"))(input)?;
    let (remainder, numbers) = separated_pair(decimals, ws(tag("|")), decimals)(input)?;
    Ok((
        remainder,
        Card {
            id,
            winning: numbers.0,
            have: numbers.1,
        },
    ))
}

fn parse(input: &str) -> IResult<&str, Vec<Card>> {
    many1(delimited(tag("Card "), parse_card, tag("\n")))(input)
}

fn count_winning(card: &Card) -> i64 {
    card.winning
        .iter()
        .filter(|n| card.have.contains(n))
        .count() as i64
}

fn task1(input: Vec<Card>) -> i64 {
    input
        .iter()
        .map(count_winning)
        .map(|n| if n == 0 { 0 } else { 2i64.pow((n - 1) as u32) })
        .sum::<i64>()
}

// fn task2(input: &str) -> String {
//     input.to_string()
// }

aoc2023::make_main!(task1, nom_parser:parse);
