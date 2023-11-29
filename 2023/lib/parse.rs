use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace1, one_of},
    combinator::{all_consuming, map_res, recognize},
    error::ParseError,
    multi::{many1, separated_list0},
    IResult, InputLength, Parser,
};

pub fn decimal(input: &str) -> IResult<&str, i64> {
    map_res(recognize(many1(one_of("0123456789"))), |out: &str| {
        out.parse()
    })(input)
}

pub fn decimals(input: &str) -> IResult<&str, Vec<i64>> {
    separated_list0(many1(alt((multispace1, tag(",")))), decimal)(input)
}

pub fn apply<I, O, E, F>(parser: F, input: I) -> O
where
    I: InputLength,
    E: ParseError<I> + std::fmt::Debug,
    F: Parser<I, O, E>,
{
    match all_consuming(parser)(input) {
        Ok((_, o)) => o,
        Err(e) => panic!("Failed to parse input: {e}"),
    }
}
