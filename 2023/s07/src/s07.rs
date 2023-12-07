use aoc2023::parse::decimal;
use nom::{
    character::complete::{newline, one_of, space1},
    multi::{many1, separated_list1},
    sequence::{separated_pair, terminated},
    IResult,
};
use std::collections::HashMap;

const TYPE_HIGH_CARD: u8 = 0;
const TYPE_ONE_PAIR: u8 = 1;
const TYPE_TWO_PAIR: u8 = 2;
const TYPE_THREE: u8 = 3;
const TYPE_FULL_HOUSE: u8 = 4;
const TYPE_FOUR: u8 = 5;
const TYPE_FIVE: u8 = 6;

struct Hand {
    cards: [u8; 5],
    ty: u8,
    bid: i64,
}

fn get_type(cards: &[u8; 5]) -> u8 {
    let mut  card_counts: HashMap<u8, usize> = HashMap::new();
    for card in cards {
        card_counts.insert(*card, card_counts.get(card).unwrap_or(&0) + 1);
    }
    if card_counts.len() == 1 {
        TYPE_FIVE;
    } else if card_counts.len() == 2 {
        let n_first = card_counts.iter().next().unwrap().1;
        if n_first == 2 || n_first == 3 {
            TYPE_FULL_HOUSE
        } else { TYPE_FOUR }
    } else if {
        // TODO not quite right with pairs
    }
}

fn parse_hand(input: &str) -> IResult<&str, Hand> {
    let (remainder, (cards, bid)) =
        separated_pair(many1(one_of("23456789TJQKA")), space1, decimal)(input)?;
    let cards: [u8; 5] = cards
        .into_iter()
        .map(|c| c as u8)
        .collect::<Vec<u8>>()
        .try_into()
        .unwrap();
    Ok((
        remainder,
        Hand {
            cards,
            ty: get_type(&cards),
            bid,
        },
    ))
}

fn parse(input: &str) -> IResult<&str, Vec<Hand>> {
    terminated(separated_list1(newline, parse_hand), newline)(input)
}

fn task1(input: Vec<Hand>) -> i64 {
    0
}

// fn task2(input: &str) -> i64 {
//     input.to_string()
// }

aoc2023::make_main!(task1, nom_parser:parse);
