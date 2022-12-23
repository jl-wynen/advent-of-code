use aoc2022::array::Array2D;
use aoc2022::render::Render;

const INPUT: &str = include_str!("input");

fn load(input: &str) -> Vec<i32> {
    input
        .chars()
        .map(|c| if c == '<' { -1 } else { 1 })
        .collect()
}

#[derive(Clone, Debug)]
struct Rock {
    pub elems: Vec<(i32, i32)>, // order: (x, y)
}

impl Rock {
    pub fn new(number: i32) -> Self {
        match number {
            0 => Self {
                elems: vec![(0, 0), (1, 0), (2, 0), (3, 0)],
            },
            1 => Self {
                elems: vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
            },
            2 => Self {
                elems: vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
            },
            3 => Self {
                elems: vec![(0, 0), (0, 1), (0, 2), (0, 3)],
            },
            4 => Self {
                elems: vec![(0, 0), (1, 0), (0, 1), (1, 1)],
            },
            _ => panic!("Bad rock number"),
        }
    }

    pub fn new_at(number: i32, x: i32, y: i32) -> Self {
        Self::new(number).move_by(x, y)
    }

    pub fn move_by(&self, x: i32, y: i32) -> Self {
        Rock {
            elems: self
                .elems
                .iter()
                .map(|(sx, sy)| (sx + x, sy + y))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Clone, Debug)]
enum Cell {
    R,
    E,
}

impl Render for Cell {
    fn render(&self) -> String {
        match self {
            Cell::R => "#".into(),
            Cell::E => ".".into(),
        }
    }
}

impl Render for RockStack {
    fn render(&self) -> String {
        self.stack.render()
    }
}

struct RockStack {
    pub height: usize,
    pub stack: Array2D<Cell>,
}

impl RockStack {
    pub fn new(n_rocks: usize) -> Self {
        Self {
            height: 0,
            stack: Array2D::<Cell>::new(n_rocks * 3 + 3, 7, Cell::E),
        }
    }

    pub fn spawn(&self, rock_no: i32) -> Rock {
        Rock::new_at(rock_no, 2, self.height as i32 + 3)
    }

    pub fn place(&mut self, rock: &Rock) {
        for &(x, y) in &rock.elems {
            if self.out_of_bounds(x, y) {
                panic!("Cannot place, is out of bounds");
            }
            self.stack[(y as usize, x as usize)] = Cell::R;
        }
        let new_height = *rock.elems.iter().map(|(_, y)| y).max().unwrap() + 1;
        self.height = std::cmp::max(self.height, new_height as usize)
    }

    pub fn out_of_bounds(&self, x: i32, y: i32) -> bool {
        x < 0 || x >= self.stack.ncol() as i32 || y < 0 || y >= self.stack.nrow() as i32
    }

    pub fn collides(&self, x: i32, y: i32) -> bool {
        match self.stack[(y as usize, x as usize)] {
            Cell::R => true,
            Cell::E => false,
        }
    }
}

fn shift_rock<Jet: Iterator<Item = i32>>(rock: &Rock, stack: &RockStack, jet: &mut Jet) -> Rock {
    let dir = jet.next().unwrap();
    let moved = rock.move_by(dir, 0);
    if moved
        .elems
        .iter()
        .any(|&(x, y)| stack.out_of_bounds(x, y) || stack.collides(x, y))
    {
        (*rock).clone()
    } else {
        moved
    }
}

fn drop_rock(rock: &Rock, stack: &RockStack) -> Option<Rock> {
    let moved = rock.move_by(0, -1);
    if moved
        .elems
        .iter()
        .any(|&(x, y)| stack.out_of_bounds(x, y) || stack.collides(x, y))
    {
        None
    } else {
        Some(moved)
    }
}

fn drop_rocks(n: usize, jet: &[i32]) -> RockStack {
    let mut stack = RockStack::new(n);
    let mut jet = jet.iter().cycle().copied();
    for rock_no in (0..5).cycle().take(n) {
        let mut rock = stack.spawn(rock_no);

        loop {
            rock = shift_rock(&rock, &stack, &mut jet);

            match drop_rock(&rock, &stack) {
                Some(r) => rock = r,
                None => break,
            }
        }
        stack.place(&rock);
    }
    stack
}

fn main() {
    let jet = load(INPUT);
    let filled = drop_rocks(2022, &jet);
    println!("Part1: {}", filled.height);
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_INPUT: &str = include_str!("test-input");

    #[test]
    fn test1() {
        let jet = load(TEST_INPUT);
        let filled = drop_rocks(2022, &jet);
        assert_eq!(filled.height, 3068);
    }

    #[test]
    fn test2() {
        let jet = load(TEST_INPUT);
        let filled = drop_rocks(1000000000000, &jet);
        assert_eq!(filled.height, 1514285714288);
    }
}
