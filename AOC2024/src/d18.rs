use std::collections::HashSet;
use crate::utils::{a_star, AStarGraph};

pub fn part1(input: &str) -> String {
    let vec = parse_input(input);
    // let map = generate_map(&vec, 12, 6); // example
    let map = generate_map(&vec, 1024, 70);
    let (_, cost) = a_star(&map).unwrap();
    cost.to_string()
}
pub fn part2(input: &str) -> String {
    let vec = parse_input(input);
    // let result = find_first_block(vec, 12, 6); // example
    let result = find_first_block(vec, 1024, 70);
    format!("{},{}", result.0, result.1)
}

fn find_first_block(vec: Vec<Pos>, init: usize, target: isize) -> (isize, isize) {
    let mut latest_path_set: HashSet<Pos> = HashSet::new();
    for i in init..vec.len() {
        let latest_pos = vec[i-1];
        if !latest_path_set.is_empty() && !latest_path_set.contains(&latest_pos) { continue; }
        let map = generate_map(&vec, i, target);
        if let Some((path, _)) = a_star(&map) {
            latest_path_set = path.iter().cloned().collect();
        } else {
            return vec[i-1];
        }
    }
    return (0,0);
}

type Pos = (isize, isize);
const DIR: [(isize, isize);4] = [(1,0),(0,1),(-1,0),(0,-1)];
struct Map {
    pub target: isize,
    pub obstacles: HashSet<Pos>
}
impl AStarGraph<Pos> for Map {
    fn get_neighbors(&self, node: &Pos) -> Vec<(Pos, isize)> {
        let mut neighbors = Vec::new();
        for (dx, dy) in DIR {
            let nx = node.0 + dx;
            let ny = node.1 + dy;
            if nx >= 0 && nx <= self.target && ny >= 0 && ny <= self.target && !self.obstacles.contains(&(nx, ny)){
                neighbors.push(((nx, ny), 1));
            }
        }
        return neighbors;
    }

    fn get_heutistic(&self, node: &Pos) -> isize {
        let (row, col) = node;
        (row - self.target).abs() + (col - self.target).abs()
    }

    fn get_start(&self) -> Pos {
        (0,0)
    }

    fn is_goal(&self, node: &Pos) -> bool {
        node.0 == self.target && node.1 == self.target
    }
}

fn generate_map(vec: &[Pos], take: usize, target: isize) -> Map {
    let obstacles: HashSet<Pos> = vec.iter().take(take).cloned().collect();
    Map { target, obstacles }
}

fn parse_input(input: &str) -> Vec<Pos> {
    input.lines().map(|l| {
        let mut split = l.split(',');
        let x = split.next().unwrap().parse().unwrap();
        let y = split.next().unwrap().parse().unwrap();
        (x, y)
    }).collect()
}