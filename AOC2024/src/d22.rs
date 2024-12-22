use std::{collections::{HashMap, HashSet}, ops::AddAssign};

pub fn part1(input: &str) -> String {
    let nums = parse_input(input);
    nums.iter().map(|n| step_n(*n, 2000)).sum::<i32>().to_string()
}
pub fn part2(input: &str) -> String {
    let nums = parse_input(input);
    let mut price_map: HashMap<ChangeTuple, u32> = HashMap::new();
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

type ChangeTuple = (i8, i8, i8, i8);
fn update_price_map(init_sec: i32, price_map: &mut HashMap<ChangeTuple, u32>) {
    let mut visited: HashSet<ChangeTuple> = HashSet::new();
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
        if !visited.contains(&key) {
            visited.insert(key);
            price_map.entry(key).or_default().add_assign(d4 as u32);
        }
        (s0, s1, s2, s3, s4) = (s1, s2, s3, s4, step(s4));
    }
}

fn parse_input(input: &str) -> Vec<i32> {
    input.lines().map(|l| l.parse().unwrap()).collect()
}