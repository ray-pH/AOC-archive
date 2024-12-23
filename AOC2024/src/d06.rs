use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let (map, guard_pos) = parse_input(input);
    let visited = get_visited_set(&map, guard_pos);
    return visited.len().to_string();
}

pub fn part2(input: &str) -> String {
    let (map, guard_pos) = parse_input(input);
    let visited = get_visited_set(&map, guard_pos);
    let loop_obs_count: usize = visited.iter()
        .filter(|(row, col)|{
            if (*row, *col) == guard_pos { return false; }
            let mut new_map = map.clone();
            if !new_map.column.contains_key(col) { new_map.column.insert(*col, Vec::new()); }
            if !new_map.row.contains_key(row) { new_map.row.insert(*row, Vec::new()); }
            insert_sorted(new_map.column.get_mut(col).unwrap(), *row);
            insert_sorted(new_map.row.get_mut(row).unwrap(), *col);
            is_looping(&new_map, guard_pos)
        })
        .count();
    return loop_obs_count.to_string();
}

fn insert_sorted<T: Ord>(vec: &mut Vec<T>, element: T) {
    match vec.binary_search(&element) {
        Ok(pos) | Err(pos) => vec.insert(pos, element),
    }
}

type Pos = (u8, u8);
#[derive(Clone)]
struct LineMap {
    column: HashMap<u8, Vec<u8>>,
    row: HashMap<u8, Vec<u8>>,
    col_size: u8,
    row_size: u8,
}

#[derive(Hash, PartialEq, Eq, Clone)]
enum DirType { X, Y }
type Dir = (DirType, i8);
fn rotate_dir(dir: Dir) -> Dir {
    match dir {
        (DirType::X, 1) => (DirType::Y, 1),
        (DirType::Y, 1) => (DirType::X, -1),
        (DirType::X, -1) => (DirType::Y, -1),
        (DirType::Y, -1) => (DirType::X, 1),
        _ => dir
    }
}

fn get_next_stop(map: &LineMap, pos: &Pos, dir: &Dir) -> (Pos, bool) {
    let (row, col) = pos;
    match dir {
        (DirType::X, 1) => {
            let row_vec = map.row.get(row);
            if row_vec.is_none() { return ((*row, map.col_size - 1), false); }
            let row_vec = row_vec.unwrap();
            let obs_col = row_vec.iter().find(|x| *x > col);
            if let Some(col) = obs_col {
                return ((*row, *col - 1), true);
            } else {
                return ((*row, map.col_size - 1), false);
            }
        },
        (DirType::Y, 1) => {
            let col_vec = map.column.get(col);
            if col_vec.is_none() { return ((map.row_size - 1, *col), false); }
            let col_vec = col_vec.unwrap();
            let obs_row = col_vec.iter().find(|x| *x > row);
            if let Some(row) = obs_row {
                return ((*row - 1, *col), true);
            } else {
                return ((map.row_size - 1, *col), false);
            }
        },
        (DirType::X, -1) => {
            let row_vec = map.row.get(row);
            if row_vec.is_none() { return ((*row, 0), false); }
            let row_vec = row_vec.unwrap();
            let obs_col = row_vec.iter().rev().find(|x| *x < col);
            if let Some(col) = obs_col {
                return ((*row, *col + 1), true);
            } else {
                return ((*row, 0), false);
            }
        },
        (DirType::Y, -1) => {
            let col_vec = map.column.get(col);
            if col_vec.is_none() { return ((0, *col), false); }
            let col_vec = col_vec.unwrap();
            let obs_row = col_vec.iter().rev().find(|x| *x < row);
            if let Some(row) = obs_row {
                return ((*row + 1, *col), true);
            } else {
                return ((0, *col), false);
            }
        },
        _ => (*pos, false)
    }
}

fn get_visited_vec(from: &Pos, to: &Pos) -> Vec<Pos> {
    use std::cmp::{min, max};
    if from.0 == to.0 {
        return (min(from.1, to.1)..=max(from.1, to.1)).map(|j| (from.0,j)).collect();
    }
    if from.1 == to.1 {
        return (min(from.0, to.0)..=max(from.0, to.0)).map(|i| (i,from.1)).collect();
    }
    return vec![*from, *to];
}

fn get_visited_set(map: &LineMap, init_pos: Pos) -> HashSet<Pos> {
    let mut visited: HashSet<Pos> = HashSet::new();
    let mut pos = init_pos;
    let mut dir = (DirType::Y, -1);
    loop {
        let (next_pos, cont) = get_next_stop(map, &pos, &dir);
        let visited_vec = get_visited_vec(&pos, &next_pos);
        visited.extend(visited_vec.iter());
        pos = next_pos;
        dir = rotate_dir(dir);
        if !cont { break };
    }
    return visited;
}

fn is_looping(map: &LineMap, init_pos: Pos) -> bool {
    let mut visited_state: HashSet<(Pos, Dir)> = HashSet::new();
    let mut pos = init_pos;
    let mut dir = (DirType::Y, -1);
    loop {
        visited_state.insert((pos, dir.clone()));
        let (next_pos, cont) = get_next_stop(map, &pos, &dir);
        pos = next_pos;
        dir = rotate_dir(dir);
        if visited_state.contains(&(pos, dir.clone())) { return true; }
        if !cont { break; };
    }
    return false;
}


fn parse_input(input: &str) -> (LineMap, Pos) {
    let mut map = LineMap {
        column: HashMap::new(),
        row: HashMap::new(),
        col_size: input.lines().next().unwrap().len() as u8,
        row_size: input.lines().count() as u8,
    };
    let mut guard_pos = (0,0);
    for (row, line) in input.lines().enumerate() {
        for (col, char) in line.chars().enumerate() {
            if char == '^' {
                guard_pos = (row as u8, col as u8);
            } else if char == '#' {
                map.column.entry(col as u8).or_default();
                map.row.entry(row as u8).or_default();
                map.column.get_mut(&(col as u8)).unwrap().push(row as u8);
                map.row.get_mut(&(row as u8)).unwrap().push(col as u8);
            }
        }
    }
    return (map, guard_pos);
}