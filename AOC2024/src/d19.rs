use std::collections::HashMap;

use crate::utils::starts_with;

pub fn part1(input: &str) -> String {
    let (available_patterns, targets) = parse_input(input);
    let available_map = generate_available_map(available_patterns.iter().map(|p| &p[..]).collect());
    targets.into_iter().filter(|t| is_pattern_possible(t, &available_map)).count().to_string()
}
pub fn part2(input: &str) -> String {
    let (available_patterns, targets) = parse_input(input);
    let available_map = generate_available_map(available_patterns.iter().map(|p| &p[..]).collect());
    let mut memo: Memo = HashMap::new();
    targets.into_iter().map(|t| count_pattern_possibility(&t, &available_map, &mut memo)).sum::<usize>().to_string()
}

type Pattern<'a> = &'a [char];
fn is_pattern_possible(target: &[char], avail_map: &HashMap<char, Vec<&[char]>>) -> bool {
    if target.is_empty() {
        return true;
    }
    
    if let Some(avail) = avail_map.get(&target[0]) {
        for pattern in avail {
            if starts_with(target.iter(), pattern.iter()) {
                // let new_target = target.clone().split_off(pattern.len());
                let new_target = &target[pattern.len()..];
                if is_pattern_possible(new_target, avail_map) {
                    return true;
                }
            }
        }
    }
    return false;
}

type Memo = HashMap<Vec<char>, usize>;
fn count_pattern_possibility(target: Pattern, avail_map: &HashMap<char, Vec<Pattern>>, memo: &mut Memo) -> usize {
    let target_vec = target.to_vec();
    if memo.contains_key(&target_vec) {
        return memo[&target_vec];
    }
    if target.is_empty() {
        return 1;
    } 
    let mut count = 0;
    if let Some(avail) = avail_map.get(&target[0]) {
        for pattern in avail {
            if starts_with(target.iter(), pattern.iter()) {
                // let new_target = target.clone().split_off(pattern.len());
                let new_target = &target[pattern.len()..];
                count += count_pattern_possibility(new_target, avail_map, memo);
            }
        }
    }
    memo.insert(target_vec, count);
    return count;
}

// fn generate_available_map(available_patterns: Vec<Pattern>) -> HashMap<char, Vec<Pattern>> {
//     let mut map: HashMap<char, Vec<Pattern>> = HashMap::new();
//     for pattern in available_patterns {
//         let init_char = pattern.front().unwrap();
//         map.entry(*init_char).or_default().push(pattern);
//     }
//     map
// }

// fn parse_input(input: &str) -> (Vec<Pattern>, Vec<Pattern>) {
//     let mut parts = input.split("\n\n");
//     let available_patterns = parts.next().unwrap().split(", ")
//         .map(|s| s.chars().collect()).collect();
//     let targets = parts.next().unwrap().lines()
//         .map(|s| s.chars().collect()).collect();
//     (available_patterns, targets)
// }

fn parse_input(input: &str) -> (Vec<Vec<char>>, Vec<Vec<char>>) {
    let mut parts = input.split("\n\n");
    let available_patterns = parts.next().unwrap().split(", ")
        .map(|s| s.chars().collect()).collect();
    let targets = parts.next().unwrap().lines()
        .map(|s| s.chars().collect()).collect();
    (available_patterns, targets)
}
fn generate_available_map(available_patterns: Vec<Pattern>) -> HashMap<char, Vec<Pattern>> {
    let mut map: HashMap<char, Vec<&[char]>> = HashMap::new();
    for pattern in available_patterns {
        let init_char = pattern[0];
        map.entry(init_char).or_default().push(pattern);
    }
    map
}