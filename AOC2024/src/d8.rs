use std::collections::{HashMap, HashSet};
use super::utils::Rational;

pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let mut antinodes: HashSet<Pos> = HashSet::new();
    for (_, antennas) in map.antennas_freq_map.iter() {
        antinodes.extend(get_antinodes_inside_map(antennas, &map));
    }
    return antinodes.len().to_string();
}

#[allow(unused)]
pub fn part2(input: &str) -> String {
    let map = parse_input(input);
    let mut antinodes: HashSet<Pos> = HashSet::new();
    for (_, antennas) in map.antennas_freq_map.iter() {
        antinodes.extend(get_continuous_antinodes_inside_map(antennas, &map));
    }
    return antinodes.len().to_string();
}

// this is technically incomplete based on the requirements,
// but there are hidden constarins in the input that make it work
pub fn part2b(input: &str) -> String {
    let map = parse_input(input);
    let mut antinodes: HashSet<Pos> = HashSet::new();
    for (_, antennas) in map.antennas_freq_map.iter() {
        antinodes.extend(get_more_antinodes_inside_map(antennas, &map));
    }
    return antinodes.len().to_string();
}

type Pos = (usize, usize);
struct Map {
    size_row: usize,
    size_col: usize,
    antennas_freq_map: HashMap<char, Vec<Pos>>,
}

fn is_pos_inside_map((r,c): &Pos, map: &Map) -> bool {
    (*r < map.size_row) && (*c < map.size_col)
}

fn get_antinodes_inside_map(antennas: &[Pos], map: &Map) -> HashSet<Pos> {
    let mut result = HashSet::new();
    for i in 0..antennas.len() {
        for j in i+1..antennas.len() {
            let (a, b) = get_antinodes_from_pair(&antennas[i], &antennas[j]);
            if is_pos_inside_map(&a, map) {
                result.insert(a);
            }
            if is_pos_inside_map(&b, map) {
                result.insert(b);
            }
        }
    }
    return result;
}

fn get_antinodes_from_pair(a: &Pos, b: &Pos) -> (Pos,Pos) {
    let diff = (b.0 - a.0, b.1 - a.1);
    let b_prime = (b.0 + diff.0, b.1 + diff.1);
    let a_prime = (a.0 - diff.0, a.1 - diff.1);
    (a_prime, b_prime)
}

fn get_continuous_antinodes_inside_map(antennas: &[Pos], map: &Map) -> HashSet<Pos> {
    let mut result = HashSet::new();
    for i in 0..antennas.len() {
        for j in i+1..antennas.len() {
            let antinodes = get_continous_antinodes_from_pair(&antennas[i], &antennas[j], map);
            result.extend(antinodes);
        }
    }
    return result;
}

fn get_continous_antinodes_from_pair(a: &Pos, b: &Pos, map: &Map) -> Vec<Pos> {
    // y1 = mx1 + c
    // y2 = mx2 + c
    // m = (y2 - y1) / (x2 - x1)
    // c = y1 - mx1
    assert!(b.1 - a.1 != 0);
    let mut antinodes: Vec<Pos> = Vec::new();
    let m = Rational::new(b.0 - a.0, b.1 - a.1);
    let a0 = Rational::new(a.0, 1);
    let c = a0.sub(&m.mul_scalar(a.1));
    for col in 0..map.size_col {
        // let row = m*col + c;
        let row = m.mul_scalar(col).add(&c);
        if row.is_integer() {
            let introw = row.get_integer_value();
            if introw < map.size_row {
                antinodes.push((introw, col));
            }
        }
    }
    return antinodes;
}

fn get_more_antinodes_inside_map(antennas: &[Pos], map: &Map) -> HashSet<Pos> {
    let mut result = HashSet::new();
    for i in 0..antennas.len() {
        for j in i+1..antennas.len() {
            let antinodes = get_more_antinodes_from_pair(&antennas[i], &antennas[j], map);
            result.extend(antinodes);
        }
    }
    return result;
}

fn get_more_antinodes_from_pair(a: &Pos, b: &Pos, map: &Map) -> Vec<Pos> {
    let diff = (b.0 - a.0, b.1 - a.1);
    let mut antinodes = Vec::new();
    let mut b_prime = (b.0 + diff.0, b.1 + diff.1);
    let mut a_prime = (a.0 - diff.0, a.1 - diff.1);
    loop {
        if !is_pos_inside_map(&b_prime, map) {
            break;
        }
        antinodes.push(b_prime);
        b_prime = (b_prime.0 + diff.0, b_prime.1 + diff.1);
    }
    loop {
        if !is_pos_inside_map(&a_prime, map) {
            break;
        }
        antinodes.push(a_prime);
        a_prime = (a_prime.0 - diff.0, a_prime.1 - diff.1);
    }
    return antinodes;
}

fn parse_input(input: &str) -> Map {
    let size_row = input.lines().count();
    let size_col = input.lines().next().unwrap().len();
    let mut antennas_freq_map = HashMap::new();
    for (row, line) in input.lines().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if c != '.' {
                antennas_freq_map.entry(c).or_insert_with(Vec::new);
                antennas_freq_map.get_mut(&c).unwrap().push((row, col));
            }
        }
    }
    return Map {size_row, size_col, antennas_freq_map};
}