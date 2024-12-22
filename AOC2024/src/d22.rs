use std::{collections::{HashMap, HashSet}, ops::AddAssign};

pub fn part1(input: &str) -> String {
    let nums = parse_input(input);
    nums.iter().map(|n| step_n(*n, 2000)).sum::<i32>().to_string()
}
pub fn part2(input: &str) -> String {
    let nums = parse_input(input);
    let mut price_map = HashMap::new();
    nums.iter().for_each(|n| update_price_map(*n, &mut price_map));
    price_map.values().max().unwrap().to_string()
}

fn step_n(sec: i32, n: usize) -> i32 {
    let mut sec = sec;
    for _ in 0..n {
        sec = step(sec);
    }
    sec
}

#[allow(clippy::let_and_return)]
const fn step(sec: i32) -> i32 {
    let sec = ((sec << 6) ^ sec) & 0xffffff;
    let sec = ((sec >> 5) ^ sec) & 0xffffff;
    let sec = ((sec << 11) ^ sec) & 0xffffff;
    sec
}

fn update_price_map(init_sec: i32, price_map: &mut HashMap<u32, u32>) {
    let mut visited: HashSet<u32> = HashSet::new();
    let s0 = init_sec;
    let s1 = step(s0);
    let s2 = step(s1);
    let s3 = step(s2);
    let mut s4 = step(s3);
    let (d0, d1, d2, d3, mut d4) = (s0 % 10, s1 % 10, s2 % 10, s3 % 10, s4 % 10);
    let (c1, c2, c3, c4) = ((d1 - d0) as u8, (d2 - d1) as u8, (d3 - d2) as u8, (d4 - d3) as u8);
    let mut key: u32 = (c1 as u32) << 24 | (c2 as u32) << 16 | (c3 as u32) << 8 | (c4 as u32);
    for _ in 4..2000 {
        if d4 > 1 {
            if !visited.contains(&key) {
                visited.insert(key);
                price_map.entry(key).or_default().add_assign(d4 as u32);
            }
        }
        s4 = step(s4);
        key = ( key & 0xFF_FF_FF ) << 8 | (( s4 % 10 - d4 ) as u8) as u32;
        d4 = s4 % 10;
    }
}

fn parse_input(input: &str) -> Vec<i32> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}