use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let vec = parse_input(input);
        vec.iter().map(|(vec, val)| val * get_minlen(vec, 2)).sum::<usize>().to_string()
}
pub fn part2(input: &str) -> String {
    let vec = parse_input(input);
    vec.iter().map(|(vec, val)| val * get_minlen(vec, 25)).sum::<usize>().to_string()
}

// fn print_char_vec(v: &[char]) {
//     let s = v.iter().map(|c| *c).collect::<String>();
//     println!("{}", s);
// }

fn get_minlen(key: &[char], depth: usize) -> usize {
    let r1s = get_numeric_movement(key);
    let r1minlen = r1s.iter().map(|r| r.len()).min().unwrap();
    let r1s = r1s.iter().filter(|r| r.len() == r1minlen).collect::<Vec<_>>();
    let mut cache = HashMap::new();
    r1s.iter().map(|r| get_directional_minlen(r, depth, &mut cache)).min().unwrap()
}

type State = (Vec<char>, usize);
//             segment  depth
fn get_directional_minlen(key: &[char], depth: usize, cache: &mut HashMap<State, usize>) -> usize {
    let split: Vec<_> = key.split(|s| *s == 'A').map(|s| [s, &['A']].concat()).collect();
    split.iter().take(split.len()-1).map(|s| get_directional_minlen_segment(s, depth, cache)).sum()
}
fn get_directional_minlen_segment(segment: &[char], depth: usize, cache: &mut HashMap<State, usize>) -> usize {
    let state: State = (segment.to_vec(), depth);
    if let Some(minlen) = cache.get(&state) {
        return *minlen;
    }
    
    if depth == 0 {
        return segment.len();
    }
    
    let mut result = HashSet::new();
    result.insert(Vec::new());
    let mut curr = 'A';
    for c in segment {
        let mut new_result = HashSet::new();
        let diff = vec_diff(&directional_keypad_pos(*c).unwrap(), &directional_keypad_pos(curr).unwrap());
        let pref_dir = directional_pref_dir(curr, *c);
        let movements = get_movements(diff, pref_dir);
        
        for r in result {
            for m in movements.iter() {
                let mut new_v = r.clone();
                new_v.extend(m);
                new_v.push('A');
                new_result.insert(new_v);
            }
        }
        curr = *c;
        result = new_result;
    }
    
    let minlen = result.iter().map(|key| get_directional_minlen(key, depth-1, cache)).min().unwrap();
    cache.insert(state, minlen);
    return minlen;
}

type Pos = (i32, i32);

const fn vec_diff(a: &Pos, b: &Pos) -> Pos {
    (a.0 - b.0, a.1 - b.1)
}
const fn vec_to_dir(vec: Pos) -> Option<char> {
    match vec {
        (0, 1) => Some('^'),
        (0, -1) => Some('v'),
        (-1, 0) => Some('<'),
        (1, 0) => Some('>'),
        _ => None
    }
}

fn get_numeric_movement(key: &[char]) -> HashSet<Vec<char>> {
    let mut result = HashSet::new();
    result.insert(Vec::new());
    let mut curr = 'A';
    for c in key {
        let mut new_result = HashSet::new();
        let diff = vec_diff(&numeric_keypad_pos(*c).unwrap(), &numeric_keypad_pos(curr).unwrap());
        let pref_dir = numeric_pref_dir(curr, *c);
        let movements = get_movements(diff, pref_dir);
        
        for r in result {
            for m in movements.iter() {
                let mut new_v = r.clone();
                new_v.extend(m);
                new_v.push('A');
                new_result.insert(new_v);
            }
        }
        curr = *c;
        result = new_result;
    }
    result
}

fn get_movements(vec: Pos, pref_dir: PreferredDirection) -> Vec<Vec<char>> {
    let mut result = Vec::new();
    let (x, y) = vec;
    let x_proc  = |r: &mut Vec<char>| {
        if x != 0 {
            let xsig = x.signum();
            let xcount = x / xsig;
            let xdir = vec_to_dir((xsig, 0)).unwrap();
            for _ in 0..xcount {
                r.push(xdir);
            }
        }
    };
    let y_proc = |r: &mut Vec<char>| {
        if y != 0 {
            let ysig = y.signum();
            let ycount = y / ysig;
            let ydir = vec_to_dir((0, ysig)).unwrap();
            for _ in 0..ycount {
                r.push(ydir);
            }
        }
    };
    match pref_dir {
        PreferredDirection::X => {
            let mut r = Vec::new();
            x_proc(&mut r);
            y_proc(&mut r);
            result.push(r);
        },
        PreferredDirection::Y => {
            let mut r = Vec::new();
            y_proc(&mut r);
            x_proc(&mut r);
            result.push(r);
        },
        PreferredDirection::None => {
            let mut rx = Vec::new();
            x_proc(&mut rx);
            y_proc(&mut rx);
            result.push(rx);
            
            let mut ry = Vec::new();
            y_proc(&mut ry);
            x_proc(&mut ry);
            result.push(ry);
        },
    }
    result
}

enum PreferredDirection { X, Y, None }

const fn numeric_pref_dir(from: char, to: char) -> PreferredDirection {
    use PreferredDirection::*;
    match (from, to) {
        ('0', '1') => Y,
        ('0', '4') => Y,
        ('0', '7') => Y,
        ('A', '1') => Y,
        ('A', '4') => Y,
        ('A', '7') => Y,
        ('1', '0') => X,
        ('4', '0') => X,
        ('7', '0') => X,
        ('1', 'A') => X,
        ('4', 'A') => X,
        ('7', 'A') => X,
        _ => None,
    }
}
const fn directional_pref_dir(from: char, to: char) -> PreferredDirection {
    use PreferredDirection::*;
    match (from, to) {
        ('<', '^') => X,
        ('<', 'A') => X,
        ('^', '<') => Y,
        ('A', '<') => Y,
        _ => None,
    }
}

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+
const fn numeric_keypad_pos(c: char) -> Option<Pos> {
    match c {
        '0' => Some((1, 0)),
        'A' => Some((2, 0)),
        '1' => Some((0, 1)),
        '2' => Some((1, 1)),
        '3' => Some((2, 1)),
        '4' => Some((0, 2)),
        '5' => Some((1, 2)),
        '6' => Some((2, 2)),
        '7' => Some((0, 3)),
        '8' => Some((1, 3)),
        '9' => Some((2, 3)),
        _ => None
    }
}

//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
const fn directional_keypad_pos(c: char) -> Option<Pos> {
    match c {
        '^' => Some((1, 1)),
        'A' => Some((2, 1)),
        '<' => Some((0, 0)),
        'v' => Some((1, 0)),
        '>' => Some((2, 0)),
        _ => None
    }
}

fn parse_input(input: &str) -> Vec<(Vec<char>, usize)> {
    input.lines().map(|l| {
        let vec = l.chars().collect();
        let val = l[0..3].parse().unwrap();
        (vec, val)
    }).collect()
}