use aoc2023::parse::decimal;
use nom::{
    character::complete::{newline, one_of, space1},
    multi::{many1, separated_list1},
    sequence::{separated_pair, terminated},
    IResult,
};
use std::cmp::Ordering;
use std::collections::HashMap;

const TYPE_HIGH_CARD: u8 = 0;
const TYPE_ONE_PAIR: u8 = 1;
const TYPE_TWO_PAIR: u8 = 2;
const TYPE_THREE: u8 = 3;
const TYPE_FULL_HOUSE: u8 = 4;
const TYPE_FOUR: u8 = 5;
const TYPE_FIVE: u8 = 6;

#[derive(Clone, Debug, Eq, PartialEq)]
struct Hand {
    cards: [u8; 5],
    ty: u8,
    bid: i64,
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ty.cmp(&other.ty).then_with(|| {
            self.cards
                .iter()
                .zip(other.cards.iter())
                .map(|(a, b)| a.cmp(b))
                .filter(|&o| o != Ordering::Equal)
                .next()
                .unwrap_or(Ordering::Equal)
        })
    }
}

fn card_value(card: char) -> u8 {
    match card {
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'T' => 10,
        'J' => 11,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => unreachable!(),
    }
}

fn get_type(cards: &[u8; 5]) -> u8 {
    let mut card_counts: HashMap<u8, usize> = HashMap::new();
    for card in cards {
        card_counts.insert(*card, card_counts.get(card).unwrap_or(&0) + 1);
    }
    let l = card_counts.len();
    if l == 1 {
        TYPE_FIVE
    } else if l == 2 {
        let n_first = *card_counts.iter().next().unwrap().1;
        if n_first == 2 || n_first == 3 {
            TYPE_FULL_HOUSE
        } else {
            TYPE_FOUR
        }
    } else if l == 3 {
        if card_counts.iter().any(|(_, n)| *n == 3) {
            TYPE_THREE
        } else {
            TYPE_TWO_PAIR
        }
    } else if l == 4 {
        TYPE_ONE_PAIR
    } else {
        TYPE_HIGH_CARD
    }
}

fn parse_hand(input: &str) -> IResult<&str, Hand> {
    let (remainder, (cards, bid)) =
        separated_pair(many1(one_of("23456789TJQKA")), space1, decimal)(input)?;
    let cards: [u8; 5] = cards
        .into_iter()
        .map(card_value)
        .collect::<Vec<_>>()
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

fn task1(mut input: Vec<Hand>) -> i64 {
    input.sort();
    input
        .iter()
        .enumerate()
        .fold(0, |acc, (i, hand)| acc + (i as i64 + 1) * hand.bid)
}

// fn task2(input: &str) -> i64 {
//     input.to_string()
// }

aoc2023::make_main!(task1, nom_parser:parse);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_type() {
        assert_eq!(get_type(&[1, 2, 3, 4, 5]), TYPE_HIGH_CARD);
        assert_eq!(get_type(&[1, 2, 3, 4, 3]), TYPE_ONE_PAIR);
        assert_eq!(get_type(&[2, 2, 3, 4, 3]), TYPE_TWO_PAIR);
        assert_eq!(get_type(&[2, 3, 3, 4, 3]), TYPE_THREE);
        assert_eq!(get_type(&[1, 3, 3, 1, 3]), TYPE_FULL_HOUSE);
        assert_eq!(get_type(&[1, 3, 1, 1, 1]), TYPE_FOUR);
        assert_eq!(get_type(&[1, 1, 1, 1, 1]), TYPE_FIVE);
    }
}
