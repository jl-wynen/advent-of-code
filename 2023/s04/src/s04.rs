use aoc2023::parse::{decimal, decimals, ws};
use nom::{
    bytes::complete::tag,
    multi::many1,
    sequence::{delimited, separated_pair},
    IResult,
};
use std::cmp;

#[derive(Clone, Debug)]
struct Card {
    winning: Vec<i64>,
    have: Vec<i64>,
    count: i64,
}

fn parse_card(input: &str) -> IResult<&str, Card> {
    let (input, _) = ws(decimal)(input)?;
    let (input, _) = ws(tag(":"))(input)?;
    let (remainder, numbers) = separated_pair(decimals, ws(tag("|")), decimals)(input)?;
    Ok((
        remainder,
        Card {
            winning: numbers.0,
            have: numbers.1,
            count: 1,
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

fn task1(cards: Vec<Card>) -> i64 {
    cards
        .iter()
        .map(count_winning)
        .map(|n| if n == 0 { 0 } else { 2i64.pow((n - 1) as u32) })
        .sum::<i64>()
}

fn task2(mut cards: Vec<Card>) -> i64 {
    let mut n_cards = 0i64;
    for i in 0..cards.len() {
        n_cards += cards[i].count;
        let n_winning = count_winning(&cards[i]);
        for j in i + 1..(cmp::min(cards.len(), n_winning as usize + i + 1)) {
            cards[j].count += cards[i].count;
        }
    }
    n_cards
}

aoc2023::make_main!(task1, task2, nom_parser:parse);
