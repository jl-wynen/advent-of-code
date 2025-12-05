use ndarray::{Array1, Array2, stack};
use pest::iterators::Pairs;

#[derive(pest_derive::Parser)]
#[grammar = "s04/src/grammar.pest"]
pub struct AOCParser;

fn parse(raw: Pairs<Rule>) -> Array2<u8> {
    let rows: Vec<_> = raw
        .into_iter()
        .next()
        .unwrap()
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::line => Some(
                pair.as_str()
                    .chars()
                    .map(|c| match c {
                        '.' => 0u8,
                        '@' => 1u8,
                        _ => unreachable!(),
                    })
                    .collect::<Array1<_>>(),
            ),
            _ => None,
        })
        .collect();
    let row_refs = rows.iter().map(|row| row.view()).collect::<Vec<_>>();
    stack(ndarray::Axis(0), &row_refs).unwrap()
}

fn count_in_row(wall: &Array2<u8>, i: usize, j: usize, include_center:bool)->u8 {
    let mut n = 0;
    if include_center {
        n+=wall[(i,j)];
    }
    if j > 0 {
        n += wall[(i , j - 1)];
    }
    if j < wall.ncols() - 1 {
        n += wall[(i, j + 1)];
    }
    n
}

fn is_accessible(wall: &Array2<u8>, i: usize, j: usize) -> bool {
    let mut n = 0;
    if i > 0 {
        n += count_in_row(wall, i-1, j, true);
    }
    if i < wall.nrows() - 1 {
        n += count_in_row(wall, i+1, j, true);
    }
    n += count_in_row(wall, i, j, false);
    n < 4
}

fn remove_accessible(wall: &mut Array2<u8>) ->usize {
    // copy so that removing does not affect accessibility:
    let wall_copy = wall.clone();
    wall.indexed_iter_mut().map(|((i,j),val)| {
            if *val == 1 && is_accessible(&wall_copy, i, j) {
                *val = 0;
                1
            } else {
                0
            }
        }).sum()
}

fn task1(input: Pairs<Rule>) -> i64 {
    let mut wall = parse(input);
    // let accessible: Vec<char> = wall
    //     .indexed_iter()
    //     .map(|((i, j), val)| {
    //         if *val == 0 {
    //             '.'
    //         } else if is_accessible(&wall, i, j) {
    //             'X'
    //         } else {
    //             '@'
    //         }
    //     })
    //     .collect();
    // for (i, val) in accessible.iter().enumerate() {
    //     if i % wall.ncols() == 0 && i > 0 {
    //         println!();
    //     }
    //     print!("{val}");
    // }
    remove_accessible(&mut wall) as i64
}

fn task2(input: Pairs<Rule>) -> i64 {
    let mut wall = parse(input);
    let mut removed = 0;
    loop {
        let n = remove_accessible(&mut wall);
        if n == 0 {
            break;
        }
        removed += n;
    }
    removed as i64
}

aoc2025::make_main!(task1, task2, parser: AOCParser, rule: Rule::wall);
