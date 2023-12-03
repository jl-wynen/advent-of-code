use itertools::Itertools;
use std::char;
use std::fmt;

#[derive(Clone, Debug)]
struct Number {
    value: i64,
    row: isize,
    first: isize,
    last: isize,
}

#[derive(Clone, Debug)]
struct SymbolTable {
    symbols: Vec<Vec<u8>>,
}

impl SymbolTable {
    fn is_symbol(&self, row: isize, col: isize) -> bool {
        if row < 0 || col < 0 {
            false
        } else {
            let row = row as usize;
            let col = col as usize;
            self.symbols
                .get(row)
                .map(|r| r.get(col).map_or(false, |&b| b != b'.'))
                .unwrap_or(false)
        }
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.symbols {
            for c in row {
                write!(f, "{}", char::from(*c))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn parse_numbers(input: &str) -> Vec<Number> {
    input
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            line.chars()
                .enumerate()
                .group_by(|(_, c)| char::is_numeric(*c))
                .into_iter()
                .filter_map(|(key, item)| if key { Some(item) } else { None })
                .map(|group| {
                    let mut it = group.into_iter();
                    let (first, c) = it.next().unwrap();
                    let mut s = String::from(c);
                    s.extend(it.map(|(_, c)| c));
                    let last = first + s.len() - 1;
                    Number {
                        value: s.parse().unwrap(),
                        row: row as isize,
                        first: first as isize,
                        last: last as isize,
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn parse_symbols(input: &str) -> SymbolTable {
    SymbolTable {
        symbols: input
            .lines()
            .map(|line| {
                line.as_bytes()
                    .iter()
                    .map(|&b| if b.is_ascii_digit() { b'.' } else { b })
                    .collect()
            })
            .collect(),
    }
}

fn parse(input: &str) -> (Vec<Number>, SymbolTable) {
    (parse_numbers(input), parse_symbols(input))
}

fn has_adjacent_symbol(number: &Number, symbols: &SymbolTable) -> bool {
    (number.row - 1..=number.row + 1)
        .any(|row| (number.first - 1..=number.last + 1).any(|col| symbols.is_symbol(row, col)))
}

fn task1(input: &str) -> String {
    let (numbers, symbols) = parse(input);
    numbers
        .iter()
        .filter(|n| has_adjacent_symbol(n, &symbols))
        .map(|n| n.value)
        .sum::<i64>()
        .to_string()
}

// fn task2(input: &str) -> String {
//     input.to_string()
// }

aoc2023::make_main!(task1);
