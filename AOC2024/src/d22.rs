use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let nums = parse_input(input);
    nums.iter().map(|n| step_n(*n, 2000)).sum::<isize>().to_string()
}
pub fn part2(input: &str) -> String {
    let nums = parse_input(input);
    let seq_maps = nums.iter().map(|n| get_seq_map(*n)).collect::<Vec<_>>();
    let unique_keys: HashSet<ChangeTuple> = HashSet::from_iter(seq_maps.iter().flat_map(|m| m.keys().cloned()));
    
    // let mut best_seq = ChangeTuple::default();
    let mut best_price = 0;
    for key in unique_keys {
        let price: usize = seq_maps.iter().map(|m| m.get(&key).unwrap_or(&0)).sum();
        if price > best_price {
            // best_seq = key;
            best_price = price;
        }
    }
    // println!("{},{},{},{}", best_seq.0, best_seq.1, best_seq.2, best_seq.3);
    best_price.to_string()
}

fn step_n(sec: isize, n: usize) -> isize {
    let mut sec = sec;
    for _ in 0..n {
        sec = step(sec);
    }
    sec
}

#[allow(clippy::let_and_return)]
const fn step(sec: isize) -> isize {
    let sec = ((sec << 6) ^ sec) & 0xffffff;
    let sec = ((sec >> 5) ^ sec) & 0xffffff;
    let sec = ((sec << 11) ^ sec) & 0xffffff;
    sec
}

type ChangeTuple = (i8, i8, i8, i8);
fn get_seq_map(init_sec: isize) -> HashMap<ChangeTuple, usize> {
    let mut seq_map: HashMap<ChangeTuple, usize> = HashMap::new();
    let mut s0 = init_sec;
    let mut s1 = step(s0);
    let mut s2 = step(s1);
    let mut s3 = step(s2);
    let mut s4 = step(s3);
    for _ in 4..2000 {
        let (d0, d1, d2, d3, d4) = (s0 % 10, s1 % 10, s2 % 10, s3 % 10, s4 % 10);
        let c1 = d1 - d0;
        let c2 = d2 - d1;
        let c3 = d3 - d2;
        let c4 = d4 - d3;
        let key = (c1 as i8, c2 as i8, c3 as i8, c4 as i8);
        seq_map.entry(key).or_insert(d4 as usize);
        (s0, s1, s2, s3, s4) = (s1, s2, s3, s4, step(s4));
    }
    seq_map
}

fn parse_input(input: &str) -> Vec<isize> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}