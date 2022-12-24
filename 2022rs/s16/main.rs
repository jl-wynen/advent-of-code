mod fileio;

use crate::fileio::{read_input, Valve};
use std::collections::{HashMap, HashSet, VecDeque};

struct BaseNode {
    pub rate: i32,
    pub neighbours: Vec<String>,
}

fn build_base_graph(valves: &Vec<Valve>) -> HashMap<String, BaseNode> {
    let mut graph = HashMap::new();
    for valve in valves {
        graph.insert(
            valve.name.clone(),
            BaseNode {
                rate: valve.rate,
                neighbours: valve.connected.clone(),
            },
        );
    }
    graph
}

struct Neighbour {
    pub distance: i32,
    pub name: String,
}

struct Node {
    pub rate: i32,
    pub neighbours: Vec<Neighbour>,
}

fn bfs_traverse(graph: &HashMap<String, BaseNode>, start: &str) -> Vec<Neighbour> {
    let mut res = Vec::new();
    let mut queue = VecDeque::from([(0, start)]);
    let mut visited = HashSet::<&str>::new();
    while let Some((d, name)) = queue.pop_front() {
        if visited.contains(name) {
            continue;
        }
        visited.insert(name);

        let node = &graph[name];
        if node.rate != 0 && d != 0 {
            res.push(Neighbour {
                distance: d + 1,
                name: name.into(),
            })
        }
        for neighbour in &node.neighbours {
            queue.push_back((d + 1, neighbour.as_str()));
        }
    }

    res
}

fn contract_zero_nodes(base_graph: &HashMap<String, BaseNode>) -> HashMap<String, Node> {
    base_graph
        .iter()
        .filter(|&(name, node)| node.rate != 0 || name == "AA")
        .map(|(name, node)| {
            (
                name.clone(),
                Node {
                    rate: node.rate,
                    neighbours: bfs_traverse(base_graph, name),
                },
            )
        })
        .collect()
}

#[derive(Clone)]
struct ValveSequence {
    pub relieved: i32,
    pub rate: i32,
    pub time_passed: i32,
    pub total_time: i32,
}

impl ValveSequence {
    pub fn new(total_time: i32) -> Self {
        Self {
            relieved: 0,
            rate: 0,
            time_passed: 0,
            total_time,
        }
    }

    pub fn advance(&self, time: i32) -> Self {
        Self {
            relieved: self.relieved + self.rate * time,
            rate: self.rate,
            time_passed: self.time_passed + time,
            total_time: self.total_time,
        }
    }

    pub fn open(&self, rate: i32) -> Self {
        Self {
            relieved: self.relieved,
            rate: self.rate + rate,
            time_passed: self.time_passed,
            total_time: self.total_time,
        }
    }

    pub fn time_left(&self) -> i32 {
        self.total_time - self.time_passed
    }

    pub fn total_relieved(&self) -> i32 {
        self.relieved + self.rate * self.time_left()
    }

    pub fn make_nested(&self) -> Self {
        Self {
            relieved: self.relieved,
            rate: self.rate,
            time_passed: 0,
            total_time: self.total_time,
        }
    }
}

fn dfs_recurse<'a>(
    graph: &'a HashMap<String, Node>,
    name: &'a str,
    valve_sequence: &ValveSequence,
    opened: &mut HashSet<&'a str>,
    do_nested: bool,
) -> ValveSequence {
    let node = &graph[name];
    let valve_sequence = valve_sequence.open(node.rate);
    opened.insert(name);
    // traverse nested -> get valve sequence + list of open valves
    // 1. recurse w/ nested sequence
    // 2. recurse w/o nested sequence
    // use optimum of 1 and 2

    let mut optimum = valve_sequence.clone();
    for neighbour in &node.neighbours {
        if !opened.contains(neighbour.name.as_str())
            && neighbour.distance < valve_sequence.time_left()
        {
            let candidate = dfs_recurse(
                graph,
                neighbour.name.as_str(),
                &valve_sequence.advance(neighbour.distance),
                opened,
                do_nested,
            );
            if candidate.total_relieved() > optimum.total_relieved() {
                optimum = candidate;
            }
        }
    }
    opened.remove(name);
    optimum
}

fn optimal_pressure_relief_part1(graph: &HashMap<String, Node>) -> i32 {
    dfs_recurse(
        graph,
        "AA",
        &ValveSequence::new(30),
        &mut HashSet::new(),
        false,
    )
    .total_relieved()
}

fn optimal_pressure_relief_part2(graph: &HashMap<String, Node>) -> i32 {
    dfs_recurse(
        graph,
        "AA",
        &ValveSequence::new(26),
        &mut HashSet::new(),
        true,
    )
    .total_relieved()
}

fn main() {
    let valves = read_input(false);
    let base_graph = build_base_graph(&valves);
    let graph = contract_zero_nodes(&base_graph);
    println!("Part1: {}", optimal_pressure_relief_part1(&graph));
    println!("Part2: {}", optimal_pressure_relief_part2(&graph));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        let valves = read_input(true);
        let base_graph = build_base_graph(&valves);
        let graph = contract_zero_nodes(&base_graph);
        let res = optimal_pressure_relief_part1(&graph);
        assert_eq!(res, 1651);
    }

    #[test]
    fn test_part2() {
        let valves = read_input(true);
        let base_graph = build_base_graph(&valves);
        let graph = contract_zero_nodes(&base_graph);
        let res = optimal_pressure_relief_part2(&graph);
        assert_eq!(res, 1707);
    }
}
