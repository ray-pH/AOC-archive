pub fn part1(input: &str) -> String {
    let reports = parse_input(input);
    return reports.iter()
        .filter(|report| is_safe(report))
        .count().to_string()
}

pub fn part2(input: &str) -> String {
    let reports = parse_input(input);
    return reports.iter()
        .filter(|report| is_safe_after_removing_one(report))
        .count().to_string()
}

fn is_safe(arr: &[i32]) -> bool {
    is_safe_diff(arr) && (is_always_increasing(arr) || is_always_decreasing(arr))
}
fn is_always_decreasing(arr: &[i32]) -> bool {
    arr.windows(2).all(|w| w[0] > w[1])
}
fn is_always_increasing(arr: &[i32]) -> bool {
    arr.windows(2).all(|w| w[0] < w[1])
}
fn is_safe_diff(arr: &[i32]) -> bool {
    arr.windows(2).all(|w| {
        let diff = (w[1] - w[0]).abs();
        (1..=3).contains(&diff)
    })
}

fn iter_skip_index(arr: &[i32], index: usize) -> Vec<i32> {
    arr.iter().take(index).chain(arr.iter().skip(index+1)).cloned().collect()
}
fn is_safe_after_removing_one(arr: &[i32]) -> bool {
    if is_safe(arr) { return true; }
    (0..arr.len()).any(|i| is_safe(&iter_skip_index(arr, i)))
}

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    return input.lines()
        .map(|line| line.split_whitespace().map(|x| x.parse::<i32>().unwrap()).collect())
        .collect()
}