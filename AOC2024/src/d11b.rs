use std::collections::HashMap;

pub fn part1(input: &str) -> String {
    let stones = parse_input(input);
    let result: u64 = stones.iter().map(|v| count_stones_after_blink(*v, 25, &mut HashMap::new())).sum();
    return result.to_string();
}

pub fn part2(input: &str) -> String {
    let stones = parse_input(input);
    let result: u64 = stones.iter().map(|v| count_stones_after_blink(*v, 75, &mut HashMap::new())).sum();
    return result.to_string();
}

type BlinkCache = HashMap<(u64,u64), u64>;
//                         (val,count), len
fn count_stones_after_blink(val: u64, count: u64, cache: &mut BlinkCache) -> u64 {
    if count == 0 {
        return 1;
    }
    if let Some(count) = cache.get(&(val,count)) {
        return *count;
    } else {
        let c = if val == 0 {
            count_stones_after_blink(1, count-1, cache)
        } else if let Some((a,b)) = split_even_digit(val) {
            let ca = count_stones_after_blink(a, count-1, cache);
            let cb = count_stones_after_blink(b, count-1, cache);
            ca + cb
        } else {
            count_stones_after_blink(val*2024, count-1, cache)
        };
        cache.insert((val,count), c);
        return c;
    }
}

fn split_even_digit(num: u64) -> Option<(u64, u64)> {
    let digit_count = num.ilog10() + 1;
    if digit_count % 2 == 0 {
        let factor = 10u64.pow(digit_count / 2);
        Some((num / factor, num % factor))
    } else {
        None
    }
}

fn parse_input(input: &str) -> Vec<u64> {
    input.lines().next().unwrap().split_whitespace().map(|s| s.parse().unwrap()).collect()
}