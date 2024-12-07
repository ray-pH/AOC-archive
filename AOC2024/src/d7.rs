pub fn part1(input: &String) -> String {
    let eqs = parse_input(input);
    let result: u64 = eqs.iter()
        .filter(|(target, arr)| is_result_possible1(*target, 0, &arr))
        .map(|(target, _)| target)
        .sum();
    return result.to_string();
}

pub fn part2(input: &String) -> String {
    let eqs = parse_input(input);
    let result: u64 = eqs.iter()
        .filter(|(target, arr)| is_result_possible2(*target, 0, &arr))
        .map(|(target, _)| target)
        .sum();
    return result.to_string();
}

fn is_result_possible1(target: u64, acc: u64, arr: &[u64]) -> bool {
    if arr.is_empty() {
        return target == acc;
    } 
    if acc > target {
        false
    } else {
        is_result_possible1(target, acc + arr[0], &arr[1..]) ||
        is_result_possible1(target, acc * arr[0], &arr[1..])
    }
}

fn concat(a: u64, b: u64) -> u64 {
    a * 10u64.pow(b.ilog10() + 1) + b
}

fn is_result_possible2(target: u64, acc: u64, arr: &[u64]) -> bool {
    if arr.is_empty() {
        return target == acc;
    } 
    if acc > target {
        false
    } else {
        is_result_possible2(target, acc + arr[0], &arr[1..]) ||
        is_result_possible2(target, acc * arr[0], &arr[1..]) ||
        is_result_possible2(target, concat(acc, arr[0]), &arr[1..])
    }
}

fn parse_input(input: &String) -> Vec<(u64, Vec<u64>)> {
    input.lines().map(|line| {
        let mut line_split = line.split(": ");
        let result = line_split.next().unwrap().parse::<u64>().unwrap();
        let vec = line_split.next().unwrap().split(" ").map(|x| x.parse::<u64>().unwrap()).collect();
        (result, vec)
    }).collect()
}