use regex::Regex;

const TEST_INPUT: &str = include_str!("test-input");
const INPUT: &str = include_str!("input");

fn load(test: bool) -> &'static str {
    if test {
        TEST_INPUT
    } else {
        INPUT
    }
}

pub struct Valve {
    pub name: String,
    pub rate: i32,
    pub connected: Vec<String>,
}

fn parse_line(line: &str) -> Valve {
    let re = Regex::new(r"^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([\w ,]*)$")
        .unwrap();
    let caps = re.captures(line).unwrap();
    Valve {
        name: caps.get(1).unwrap().as_str().into(),
        rate: caps.get(2).unwrap().as_str().parse().unwrap(),
        connected: caps
            .get(3)
            .unwrap()
            .as_str()
            .split(", ")
            .map(|x| x.into())
            .collect(),
    }
}

pub fn read_input(test: bool) -> Vec<Valve> {
    load(test).lines().map(parse_line).collect()
}
