use std::collections::{BinaryHeap, HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let (_, cost) = a_star(&map).unwrap();
    return cost.to_string();
}
pub fn part2(input: &str) -> String {
    let map = parse_input(input);
    let points = a_star_all_points(&map);
    let points: HashSet<(i16, i16)> = HashSet::from_iter(points.iter().map(|c| (c.0, c.1)));
    return points.len().to_string();
}

// T is the coordinate type
#[derive(Debug, Clone, Eq, PartialEq)]
struct Node<T> {
    pos: T,
    cost: i32,
    est_cost: i32,
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
    fn get_neighbors(&self, node: &T) -> &Vec<(T, i32)>;
    fn get_heutistic(&self, node: &T) -> i32;
    fn get_start(&self) -> T;
    fn is_goal(&self, node: &T) -> bool;
}

fn a_star<T>(graph: &impl AStarGraph<T>) -> Option<(Vec<T>, i32)>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug,
{
    let mut open_set: BinaryHeap<Node<T>> = BinaryHeap::new();
    let mut came_from: HashMap<T, T> = HashMap::new();
    let mut g_score: HashMap<T, i32> = HashMap::new();
    
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
            if tentative_g_score <= *g_score.get(neigh).unwrap_or(&i32::MAX) {
                came_from.insert(neigh.clone(), current.pos.clone());
                g_score.insert(neigh.clone(), tentative_g_score);
                open_set.push(Node {
                    pos: neigh.clone(),
                    cost: tentative_g_score,
                    est_cost: tentative_g_score + graph.get_heutistic(neigh),
                });
            }
        }
    }
    return None;
}

fn a_star_all_points<T>(graph: &impl AStarGraph<T>) -> HashSet<T>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug,
{
    let mut open_set: BinaryHeap<Node<T>> = BinaryHeap::new();
    let mut came_from: HashMap<T, (Vec<T>, i32)> = HashMap::new();
    let mut g_score: HashMap<T, i32> = HashMap::new();
    let mut goals: HashSet<T> = HashSet::new();
    let mut points: HashSet<T> = HashSet::new();
    
    let start = graph.get_start();
    open_set.push(Node {
        pos: start.clone(),
        cost: 0,
        est_cost: graph.get_heutistic(&start),
    });
    g_score.insert(start, 0);
    
    while let Some(current) = open_set.pop() {
        if graph.is_goal(&current.pos) {
            goals.insert(current.pos.clone());
        }
        for (neigh, cost) in graph.get_neighbors(&current.pos) {
            let tentative_g_score = g_score[&current.pos] + cost;
            if tentative_g_score <= *g_score.get(neigh).unwrap_or(&i32::MAX) {
                g_score.insert(neigh.clone(), tentative_g_score);
                open_set.push(Node {
                    pos: neigh.clone(),
                    cost: tentative_g_score,
                    est_cost: tentative_g_score + graph.get_heutistic(neigh),
                });
                // came_from.insert(neigh.clone(), current.pos.clone());
                // let came_from_data = came_from.get(&neigh);
                if let Some(came_from_data) = came_from.get_mut(neigh) {
                    #[allow(clippy::comparison_chain)]
                    if came_from_data.1 == tentative_g_score {
                        // same, add
                        came_from_data.0.push(current.pos.clone());
                    } else if came_from_data.1 > tentative_g_score {
                        // overwrite
                        came_from.insert(neigh.clone(), (vec![current.pos.clone()], tentative_g_score));
                    }
                } else {
                    came_from.insert(neigh.clone(), (vec![current.pos.clone()], tentative_g_score));
                }
            }
        }
    }
    
    let mut heads: Vec<T> = goals.iter().cloned().collect();
    let mut visited_heads: HashSet<T> = HashSet::new();
    while let Some(head) = heads.pop() {
        if visited_heads.contains(&head) {
            continue;
        }
        visited_heads.insert(head.clone());
        if let Some((froms, _)) = came_from.get(&head) {
            heads.extend(froms.iter().cloned());
            points.insert(head);
        }
    }
    
    return points;
}


type Coord = (i16, i16, i16);
struct Map {
    pub connection_map: HashMap<Coord, Vec<(Coord, i32)>>,
    pub start: Coord,
    pub goal: (i16, i16),
}
impl AStarGraph<Coord> for Map {
    fn get_neighbors(&self, node: &Coord) -> &Vec<(Coord, i32)> {
        self.connection_map.get(node).unwrap()
    }

    fn get_heutistic(&self, node: &Coord) -> i32 {
        let (row, col, _) = node;
        (row - self.goal.0).abs() as i32 + (col - self.goal.1).abs() as i32
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
fn parse_input(input: &str) -> Map {
    let mut start = (0,0,0);
    let mut goal = (0,0);
    let mut connection_map: HashMap<Coord, Vec<(Coord, i32)>> = HashMap::new();
    let board = input.lines().map(|l| l.chars().collect()).collect::<Vec<Vec<char>>>();
    for (row, line) in input.lines().enumerate() {
        for (col, s) in line.chars().enumerate() {
            let r = row as i16;
            let c = col as i16;
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
            let r = row as i16;
            let c = col as i16;
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