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
    fn get_symbol(&self, row: isize, col: isize) -> Option<u8> {
        if row < 0 || col < 0 {
            None
        } else {
            let row = row as usize;
            let col = col as usize;
            self.symbols.get(row).and_then(|r| {
                r.get(col)
                    .and_then(|&b| if b != b'.' { Some(b) } else { None })
            })
        }
    }

    fn is_symbol(&self, row: isize, col: isize) -> bool {
        self.get_symbol(row, col).is_some()
    }

    fn is_potential_gear(&self, row: isize, col: isize) -> bool {
        self.get_symbol(row, col).map_or(false, |b| b == b'*')
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

fn task1((numbers, symbols): (Vec<Number>, SymbolTable)) -> i64 {
    numbers
        .iter()
        .filter(|n| has_adjacent_symbol(n, &symbols))
        .map(|n| n.value)
        .sum::<i64>()
}

fn get_potential_adjacent_gears(number: &Number, symbols: &SymbolTable) -> Vec<(isize, isize)> {
    (number.row - 1..=number.row + 1)
        .flat_map(|row| {
            (number.first - 1..=number.last + 1).filter_map(move |col| {
                if symbols.is_potential_gear(row, col) {
                    Some((row, col))
                } else {
                    None
                }
            })
        })
        .collect()
}

fn task2((numbers, symbols): (Vec<Number>, SymbolTable)) -> i64 {
    let mut gears: Vec<(isize, isize, Vec<i64>)> = Vec::new();
    for number in numbers {
        for (row, col) in get_potential_adjacent_gears(&number, &symbols) {
            if let Some(item) = gears.iter_mut().find(|(r, c, _)| *r == row && *c == col) {
                item.2.push(number.value);
            } else {
                gears.push((row, col, vec![number.value]));
            }
        }
    }
    gears
        .iter()
        .filter(|(_, _, values)| values.len() == 2)
        .map(|(_, _, values)| values.iter().product::<i64>())
        .sum::<i64>()
}

aoc2023::make_main!(task1, task2, parser:parse);
