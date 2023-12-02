use aoc2023::parse::decimal;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map_res,
    error,
    multi::{many1, separated_list1},
    sequence::{delimited, pair},
    IResult,
};

#[derive(Debug, Clone, Default)]
struct Draw {
    red: i64,
    green: i64,
    blue: i64,
}

#[derive(Debug, Clone)]
struct Game {
    id: i64,
    draws: Vec<Draw>,
}

fn single_colour_draw((n, colour): (i64, &str)) -> Result<Draw, ()> {
    Ok(match colour {
        " red" => Draw {
            red: n,
            ..Default::default()
        },
        " green" => Draw {
            green: n,
            ..Default::default()
        },
        " blue" => Draw {
            blue: n,
            ..Default::default()
        },
        _ => unreachable!(),
    })
}

fn parse_draw(input: &str) -> IResult<&str, Draw> {
    map_res(
        separated_list1(
            tag(", "),
            map_res(
                pair(decimal, alt((tag(" red"), tag(" green"), tag(" blue")))),
                single_colour_draw,
            ),
        ),
        |draws| {
            Ok::<Draw, error::Error<&str>>(draws.into_iter().fold(Draw::default(), |acc, draw| {
                Draw {
                    red: acc.red + draw.red,
                    green: acc.green + draw.green,
                    blue: acc.blue + draw.blue,
                }
            }))
        },
    )(input)
}

fn parse_game(input: &str) -> IResult<&str, Game> {
    let (input, id) = decimal(input)?;
    let (input, _) = tag(": ")(input)?;
    let (remainder, draws) = separated_list1(tag("; "), parse_draw)(input)?;
    Ok((remainder, Game { id, draws: draws }))
}

fn parse(input: &str) -> IResult<&str, Vec<Game>> {
    many1(delimited(tag("Game "), parse_game, tag("\n")))(input)
}

fn task1(input: Vec<Game>) -> String {
    const MAX_RED: i64 = 12;
    const MAX_GREEN: i64 = 13;
    const MAX_BLUE: i64 = 14;
    input
        .iter()
        .filter(|game| {
            game.draws
                .iter()
                .all(|draw| draw.red <= MAX_RED && draw.green <= MAX_GREEN && draw.blue <= MAX_BLUE)
        })
        .map(|game| game.id)
        .sum::<i64>()
        .to_string()
}

// fn task2(input: &str) -> String {
//     input.to_string()
// }

aoc2023::make_main!(task1, nom_parser: parse);
