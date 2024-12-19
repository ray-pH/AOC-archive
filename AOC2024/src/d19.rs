use std::collections::{HashMap, LinkedList};

use crate::utils::starts_with;

pub fn part1(input: &str) -> String {
    let (available_patterns, targets) = parse_input(input);
    let available_map = generate_available_map(available_patterns);
    targets.into_iter().filter(|t| is_pattern_possible(t.clone(), &available_map)).count().to_string()
}
pub fn part2(input: &str) -> String {
    let (available_patterns, targets) = parse_input(input);
    let available_map = generate_available_map(available_patterns);
    let mut memo: Memo = HashMap::new();
    targets.into_iter().map(|t| count_pattern_possibility(t.clone(), &available_map, &mut memo)).sum::<usize>().to_string()
}

type Pattern = LinkedList<char>;

fn is_pattern_possible(target: Pattern, avail_map: &HashMap<char, Vec<Pattern>>) -> bool {
    if target.is_empty() {
        return true;
    }
    
    if let Some(avail) = avail_map.get(target.front().unwrap()) {
        for pattern in avail {
            if starts_with(target.iter(), pattern.iter()) {
                let new_target = target.clone().split_off(pattern.len());
                if is_pattern_possible(new_target, avail_map) {
                    return true;
                }
            }
        }
    }
    return false;
}

type Memo = HashMap<Pattern, usize>;
fn count_pattern_possibility(target: Pattern, avail_map: &HashMap<char, Vec<Pattern>>, memo: &mut Memo) -> usize {
    if memo.contains_key(&target) {
        return memo[&target];
    }
    if target.is_empty() {
        return 1;
    } 
    let mut count = 0;
    if let Some(avail) = avail_map.get(target.front().unwrap()) {
        for pattern in avail {
            if starts_with(target.iter(), pattern.iter()) {
                let new_target = target.clone().split_off(pattern.len());
                count += count_pattern_possibility(new_target, avail_map, memo);
            }
        }
    }
    memo.insert(target, count);
    return count;
}

fn generate_available_map(available_patterns: Vec<Pattern>) -> HashMap<char, Vec<Pattern>> {
    let mut map: HashMap<char, Vec<Pattern>> = HashMap::new();
    for pattern in available_patterns {
        let init_char = pattern.front().unwrap();
        map.entry(*init_char).or_default().push(pattern);
    }
    map
}

fn parse_input(input: &str) -> (Vec<Pattern>, Vec<Pattern>) {
    let mut parts = input.split("\n\n");
    let available_patterns = parts.next().unwrap().split(", ")
        .map(|s| s.chars().collect()).collect();
    let targets = parts.next().unwrap().lines()
        .map(|s| s.chars().collect()).collect();
    (available_patterns, targets)
}