use std::collections::{BinaryHeap, HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let (_, cost) = a_star(&map).unwrap();
    return cost.to_string();
}
pub fn part2(input: &str) -> String {
    let map = parse_input(input);
    let (path, _) = a_star(&map).unwrap();
    let (mut cost_map, explorers) = generate_cost_map_exp(path);
    let coords = get_all_optimal_points(&map, &mut cost_map, explorers);
    let mut points: HashSet<(isize, isize)> = HashSet::from_iter(coords.iter().map(|c| (c.0, c.1)));
    points.insert(map.goal);
    return points.len().to_string();
}

// T is the coordinate type
#[derive(Debug, Clone, Eq, PartialEq)]
struct Node<T> {
    pos: T,
    cost: isize,
    est_cost: isize,
}
impl<T: Eq> Ord for Node<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.est_cost.cmp(&self.est_cost)
        .then_with(|| self.cost.cmp(&other.cost))
    }
}
impl<T: Eq> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

trait AStarGraph<T> {
    fn get_neighbors(&self, node: &T) -> &Vec<(T, isize)>;
    fn get_heutistic(&self, node: &T) -> isize;
    fn get_start(&self) -> T;
    fn is_goal(&self, node: &T) -> bool;
}

fn a_star<T>(graph: &impl AStarGraph<T>) -> Option<(Vec<T>, isize)>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug,
{
    let mut open_set: BinaryHeap<Node<T>> = BinaryHeap::new();
    let mut came_from: HashMap<T, T> = HashMap::new();
    let mut g_score: HashMap<T, isize> = HashMap::new();
    
    let start = graph.get_start();
    open_set.push(Node {
        pos: start.clone(),
        cost: 0,
        est_cost: graph.get_heutistic(&start),
    });
    g_score.insert(start, 0);
    
    while let Some(current) = open_set.pop() {
        if graph.is_goal(&current.pos) {
            // reconstruct the path
            let mut path = vec![current.pos];
            while let Some(prev) = came_from.get(&path[0]) {
                path.insert(0, prev.clone());
            }
            return Some((path, current.cost));
        }
        for (neigh, cost) in graph.get_neighbors(&current.pos) {
            let tentative_g_score = g_score[&current.pos] + cost;
            if tentative_g_score <= *g_score.get(&neigh).unwrap_or(&std::isize::MAX) {
                came_from.insert(neigh.clone(), current.pos.clone());
                g_score.insert(neigh.clone(), tentative_g_score);
                open_set.push(Node {
                    pos: neigh.clone(),
                    cost: tentative_g_score,
                    est_cost: tentative_g_score + graph.get_heutistic(&neigh),
                });
            }
        }
    }
    return None;
}


type Coord = (isize, isize, isize);
struct Map {
    pub connection_map: HashMap<Coord, Vec<(Coord, isize)>>,
    pub start: Coord,
    pub goal: (isize, isize),
}
impl AStarGraph<Coord> for Map {
    fn get_neighbors(&self, node: &Coord) -> &Vec<(Coord, isize)> {
        self.connection_map.get(node).unwrap()
    }

    fn get_heutistic(&self, node: &Coord) -> isize {
        let (row, col, _) = node;
        (row - self.goal.0).abs() + (col - self.goal.1).abs()
    }

    fn get_start(&self) -> Coord {
        self.start
    }

    fn is_goal(&self, node: &Coord) -> bool {
        node.0 == self.goal.0 && node.1 == self.goal.1
    }
}

struct Explorer {
    cost: isize,
    path: Vec<Coord>,
}
fn generate_cost_map_exp(path: Vec<Coord>) -> (HashMap<Coord, isize>, Vec<Explorer>) {
    let mut cost_map = HashMap::new();
    let mut explorers = Vec::new();
    cost_map.insert(path[0], 0);
    explorers.push(Explorer { cost: 0, path: vec![path[0].clone()]});
    for w in path.windows(2) {
        let a = w[0];
        let b = w[1];
        let prev_cost = cost_map[&a];
        let d_cost = if a.2 == b.2 { 1 } else { 1000 };
        let new_cost = prev_cost + d_cost;
        explorers.push(Explorer { cost: new_cost, path: vec![b.clone()]});
        cost_map.insert(b, new_cost);
    }
    return (cost_map, explorers);
}

fn get_all_optimal_points(graph: &impl AStarGraph<Coord>, cost_map: &mut HashMap<Coord, isize>, explorers: Vec<Explorer>) -> HashSet<Coord> {
    let mut points = HashSet::new();
    let mut explorers = explorers;
    
    let optimal_path_set: HashSet<Coord> = cost_map.keys().cloned().collect();
    
    while let Some(exp) = explorers.pop() {
        let head = exp.path.last().unwrap();
        for (neigh, cost) in graph.get_neighbors(head) {
            let tentative_cost = exp.cost + cost;
            let neigh_cost = cost_map.get(&neigh).unwrap_or(&std::isize::MAX);
            if tentative_cost == *neigh_cost && optimal_path_set.contains(neigh) {
                // inside optimal
                points.extend(exp.path.iter());
            }
            else if tentative_cost <= *neigh_cost {
                // new explorer
                let mut new_path = exp.path.clone();
                new_path.push(neigh.clone());
                explorers.push(Explorer {
                    cost: tentative_cost,
                    path: new_path,
                });
                cost_map.insert(*neigh, tentative_cost);
            }
        }
    }
    
    return points;
}

fn is_empty_char(c: char) -> bool {
    c == '.' || c == 'S' || c == 'E'
}
fn parse_input(input: &str) -> Map {
    let mut start = (0,0,0);
    let mut goal = (0,0);
    let mut connection_map: HashMap<Coord, Vec<(Coord, isize)>> = HashMap::new();
    let board = input.lines().map(|l| l.chars().collect()).collect::<Vec<Vec<char>>>();
    for (row, line) in input.lines().enumerate() {
        for (col, s) in line.chars().enumerate() {
            let r = row as isize;
            let c = col as isize;
            if is_empty_char(s) {
                // horizontal (0)
                if is_empty_char(board[row][col+1]){
                    connection_map.entry((r, c, 0)).or_default().push(((r, c + 1, 0), 1));
                }
                if is_empty_char(board[row][col-1]){
                    connection_map.entry((r, c, 0)).or_default().push(((r, c - 1, 0),1));
                } 
                // vertical (1)
                if is_empty_char(board[row+1][col]){
                    connection_map.entry((r, c, 1)).or_default().push(((r + 1, c, 1),1));
                }
                if is_empty_char(board[row-1][col]){
                    connection_map.entry((r, c, 1)).or_default().push(((r - 1, c, 1),1));
                }
            }
            if s == 'S' {
                start = (r, c, 0)
            }
            if s == 'E' {
                goal = (r, c);
            }
        }
    }
    for (row, line) in input.lines().enumerate() {
        for (col, s) in line.chars().enumerate() {
            let r = row as isize;
            let c = col as isize;
            if is_empty_char(s) {
                let is_horizontal = connection_map.contains_key(&(r, c, 0));
                let is_vertical = connection_map.contains_key(&(r, c, 1));
                if is_horizontal && is_vertical {
                    connection_map.entry((r,c,0)).or_default().push(((r, c, 1), 1000));
                    connection_map.entry((r,c,1)).or_default().push(((r, c, 0), 1000));
                }
            }
        }
    }
    return Map { connection_map, start, goal };
}