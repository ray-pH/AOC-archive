use std::collections::{HashMap, HashSet};

use crate::utils::{a_star2, AStarGraph2};

pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let (_, cost) = a_star2(&map).unwrap();
    // let finished = explore_and_get_finished(&map, cost as usize);
    let finished = explore_and_get_finished(&map, cost as usize - 100 + 1);
    finished.to_string()
}
pub fn part2(input: &str) -> String {
    todo!();
}

type Coord = (isize, isize);
struct Map {
    pub connection_map: HashMap<Coord, Vec<(Coord, isize)>>,
    pub walls: HashSet<Coord>,
    pub start: Coord,
    pub goal: (isize, isize),
}


#[derive(Hash, PartialEq, Eq)]
struct Explorer {
    has_cheated: bool,
    pos: Coord,
    prev_pos: Option<Coord>,
}
fn explore_and_get_finished(map: &Map, max_steps: usize) -> usize {
    let mut finished = 0;
    let mut explorers: FrequencyMap<Explorer> = FrequencyMap::new();
    explorers.add(Explorer{ has_cheated: false, pos: map.start, prev_pos: None}, 1);
    for i in 0..max_steps {
        let (new_explorers, new_finished) = step(map, &explorers, max_steps - i);
        finished += new_finished;
        explorers = new_explorers;
        println!("{}, {}, {}", max_steps - i, explorers.map.len(), new_finished);
    }
    return finished;
}

fn step(map: &Map, explorers: &FrequencyMap<Explorer>, limit: usize) -> (FrequencyMap<Explorer>, usize) {
    let mut new_explorers: FrequencyMap<Explorer> = FrequencyMap::new();
    let mut finished = 0;
    for (explorer, count) in explorers.map.iter() {
        if map.is_goal(&explorer.pos) {
            finished += count;
            continue;
        }
        if map.get_heutistic(&explorer.pos) > limit as isize {
            continue;
        }
        if explorer.has_cheated {
            for (neigh, _) in map.get_neighbors(&explorer.pos) {
                if Some(*neigh) == explorer.prev_pos { continue; }
                let new_exp = Explorer{ has_cheated: true, pos: *neigh, prev_pos: Some(explorer.pos)};
                new_explorers.add(new_exp, *count);
            }
        } else {
            for (dr, dc) in DIR {
                let nr = explorer.pos.0 + dr;
                let nc = explorer.pos.1 + dc;
                let npos = (nr, nc);
                if Some(npos) == explorer.prev_pos { continue; }
                let is_wall = map.walls.contains(&npos); 
                if is_wall {
                    let nnpos = (nr + dr, nc + dc);
                    if map.walls.contains(&nnpos) { continue; }
                }
                let new_exp = Explorer{ has_cheated: is_wall, pos: npos, prev_pos: Some(explorer.pos)};
                new_explorers.add(new_exp, *count);
            }
        }
    }
    return (new_explorers, finished);
}

struct FrequencyMap<T> {
    pub map: HashMap<T, usize>,
}
impl<T> FrequencyMap<T>
where
    T: std::hash::Hash + Eq, // HashMap requires these traits for keys
{
    fn new() -> Self {
        Self { map: HashMap::new() }
    }
    fn add(&mut self, element: T, value: usize) {
        *self.map.entry(element).or_default() += value
    }
}



impl AStarGraph2<Coord> for Map {
    fn get_neighbors(&self, node: &Coord) -> &Vec<(Coord, isize)> {
        self.connection_map.get(node).unwrap()
    }

    fn get_heutistic(&self, node: &Coord) -> isize {
        let (row, col) = node;
        (row - self.goal.0).abs() + (col - self.goal.1).abs()
    }

    fn get_start(&self) -> Coord {
        self.start
    }

    fn is_goal(&self, node: &Coord) -> bool {
        node.0 == self.goal.0 && node.1 == self.goal.1
    }
}

fn is_empty_char(c: char) -> bool {
    c == '.' || c == 'S' || c == 'E'
}
const DIR: [(isize, isize);4] = [(1,0),(0,1),(-1,0),(0,-1)];
fn parse_input(input: &str) -> Map {
    let mut start = (0,0);
    let mut goal = (0,0);
    let mut connection_map: HashMap<Coord, Vec<(Coord, isize)>> = HashMap::new();
    let mut walls: HashSet<Coord> = HashSet::new();
    let board = input.lines().map(|l| l.chars().collect()).collect::<Vec<Vec<char>>>();
    for (row, line) in input.lines().enumerate() {
        for (col, s) in line.chars().enumerate() {
            let r = row as isize;
            let c = col as isize;
            // if is_empty_char(s) {
            for (dr, dc) in DIR {
                let nr = r + dr;
                let nc = c + dc;
                let is_inside = nr >= 0 && nr < board.len() as isize && nc >= 0 && nc < board[0].len() as isize;
                if is_inside && is_empty_char(board[nr as usize][nc as usize]) {
                    connection_map.entry((r, c)).or_default().push(((nr, nc), 1));
                }
            }
            // }
            if s == '#' { walls.insert((r, c)); }
            if s == 'S' { start = (r, c) }
            if s == 'E' { goal = (r, c); }
        }
    }
    return Map { connection_map, start, goal, walls };
}