use std::iter::zip;

pub fn part1(input: &String) -> String {
    let (mut left, mut right) = parse_input(input);
    left.sort();
    right.sort();
    let result = zip(left, right).map(|(a, b)| (a-b).abs()).sum::<i32>();
    return result.to_string();
}
pub fn part2(input: &String) -> String {
    let (left, right) = parse_input(input);
    let result = left.iter()
        .map(|x| x * right.iter().filter(|y| &x == y).count() as i32)
        .sum::<i32>();
    return result.to_string();
}

fn parse_input(input: &String) -> (Vec<i32>, Vec<i32>) {
    let mut left = Vec::new();
    let mut right = Vec::new();
    for line in input.lines() {
        let mut parts = line.split_whitespace();
        let left_el = parts.next().unwrap().parse::<i32>().unwrap();
        let right_el = parts.next().unwrap().parse::<i32>().unwrap();
        left.push(left_el);
        right.push(right_el);
    }
    return (left, right);
}