use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let (path_set, start, end) = parse_input(input);
    let path_vec = gen_path(&path_set, start, end);
    let cost_map = gen_cost_map(&path_vec);
    count_cheats1(&path_vec, cost_map, 100).to_string()
}
pub fn part2(input: &str) -> String {
    let (path_set, start, end) = parse_input(input);
    let path_vec = gen_path(&path_set, start, end);
    let cost_vec = gen_cost_vec(&path_vec);
    count_cheats2(&cost_vec, 20, 100).to_string()
}

type Coord = (isize, isize);
fn manhattan(a: &Coord, b: &Coord) -> usize {
    ((a.0 - b.0).abs() + (a.1 - b.1).abs()) as usize
}
fn count_cheats2(fair_path_cost: &[(Coord, isize)], cheat_length: isize, min_saving: isize) -> usize {
    let mut count = 0;
    for i in 0..fair_path_cost.len() {
        for j in i+min_saving as usize..fair_path_cost.len() {
            let (a, a_cost) = fair_path_cost[i];
            let (b, b_cost) = fair_path_cost[j];
            let dist = manhattan(&a, &b) as isize;
            if dist <= cheat_length && (a_cost - b_cost) - dist >= min_saving {
                count += 1;
            }
        }
    }
    return count;
}

const KERNEL: [(isize, isize);4] = [(2,0),(0,2),(-2,0),(0,-2)];
fn count_cheats1(path_vec: &[Coord], cost_map: HashMap<Coord, isize>, min_saving: isize) -> usize {
    let mut count = 0;
    for pos in path_vec {
        for dpos in KERNEL {
            let cost = cost_map.get(&pos).unwrap();
            let npos = (pos.0 + dpos.0, pos.1 + dpos.1);
            if let Some(ncost) = cost_map.get(&npos) {
                let saving = cost - ncost - 2;
                if saving >= min_saving {
                    count += 1;
                }
            }
        }
    }
    return count;
}

struct Explorer {
    pos: Coord,
    prev: Option<Coord>,
}
const DIR: [(isize, isize);4] = [(1,0),(0,1),(-1,0),(0,-1)];
fn gen_path(map: &HashSet<Coord>, start: Coord, end: Coord) -> Vec<Coord> {
    let mut path: Vec<Coord> = Vec::new();
    let mut exp = Explorer { pos: start, prev: None };
    while exp.pos != end {
        path.push(exp.pos);
        for dpos in DIR {
            let npos = (exp.pos.0 + dpos.0, exp.pos.1 + dpos.1);
            if map.contains(&npos) && Some(npos) != exp.prev {
                exp.prev = Some(exp.pos);
                exp.pos = npos;
                break;
            }
        }
    }
    path.push(end);
    return path;
}

fn gen_cost_map(vec: &[Coord]) -> HashMap<Coord, isize> {
    let mut map: HashMap<Coord, isize> = HashMap::new();
    for (i, pos) in vec.iter().enumerate() {
        let cost = vec.len() - i - 1;
        map.insert(*pos, cost as isize);
    }
    return map;
}
fn gen_cost_vec(vec: &[Coord]) -> Vec<(Coord, isize)> {
    let mut v: Vec<(Coord, isize)> = Vec::new();
    for (i, pos) in vec.iter().enumerate() {
        let cost = vec.len() - i - 1;
        v.push((*pos, cost as isize));
    }
    return v;
}

fn is_empty_char(c: char) -> bool {
    c == '.' || c == 'S' || c == 'E'
}
fn parse_input(input: &str) -> (HashSet<Coord>, Coord, Coord) {
    let mut start = (0,0);
    let mut goal = (0,0);
    let mut path: HashSet<Coord> = HashSet::new();
    for (row, line) in input.lines().enumerate() {
        for (col, s) in line.chars().enumerate() {
            let pos = (row as isize, col as isize);
            if is_empty_char(s) {
                path.insert(pos);
            }
            if s == 'S' { start = pos; }
            if s == 'E' { goal = pos; }
        }
    }
    return (path, start, goal);
}