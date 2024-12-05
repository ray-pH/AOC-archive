use std::collections::{HashMap, HashSet};

pub fn part1(input: &String) -> String {
    let (rules, pages_arr) = parse_input(input);
    let srule = simplify_rule(&rules);
    let result: usize = pages_arr.iter()
        .filter(|pages| is_pages_valid(&srule, pages))
        .map(|pages| get_middle_page(pages))
        .sum();
    return result.to_string();
}

pub fn part2(input: &String) -> String {
    let (rules, pages_arr) = parse_input(input);
    let srule = simplify_rule(&rules);
    let invalid_pages_arr: Vec<Pages> = pages_arr.into_iter().filter(|pages| !is_pages_valid(&srule, pages)).collect();
    
    let result: usize = invalid_pages_arr.iter()
        .map(|pages| filter_usable_rules(&rules, &pages))
        .map(|r| generate_ordering(&r))
        .map(|ordering| get_middle_page(&ordering))
        .sum();
    return result.to_string();
}

type Rule = (usize,usize);
type Pages = Vec<usize>;
type SimplifiedRule = HashMap<usize, Vec<usize>>; // page -> vec of later pages that turn the pages invalid

fn simplify_rule(rules: &Vec<Rule>) -> SimplifiedRule {
    let mut sr: SimplifiedRule = HashMap::new();
    for (a,b) in rules {
        if let Some(vec) = sr.get_mut(b) {
            vec.push(*a);
        } else {
            sr.insert(*b, vec![*a]);
        }
    }
    return sr;
}

fn is_pages_valid(sr: &SimplifiedRule, pages: &Pages) -> bool {
    let mut invalid_later_pages: HashSet<usize> = HashSet::new();
    for page in pages {
        if invalid_later_pages.contains(page) {
            return false;
        }
        if let Some(vec) = sr.get(page) {
            invalid_later_pages.extend(vec.iter());
        }
    }
    return true;
}

fn get_middle_page(pages: &Pages) -> usize {
    let middle = pages.len() / 2;
    return pages[middle];
}

fn filter_usable_rules(rules: &Vec<Rule>, pages: &Pages) -> Vec<Rule> {
    let pages_set: HashSet<usize> = HashSet::from_iter(pages.into_iter().cloned());
    return rules.iter().filter(|(a,b)| pages_set.contains(a) && pages_set.contains(b)).cloned().collect();
}

fn generate_ordering(rules: &Vec<Rule>) -> Vec<usize> {
    let mut pool = rules.clone();
    let mut ordering = Vec::new();
    while pool.len() > 1 {
        let smallest = get_smallest(&pool);
        pool = pool.iter().filter(|(a,_)| a != &smallest).cloned().collect();
        ordering.push(smallest);
    }
    let (final_a,final_b) = pool[0];
    ordering.push(final_a);
    ordering.push(final_b);
    return ordering;
}

fn get_smallest(rules: &Vec<Rule>) -> usize {
    let mut possible_smallest: HashSet<usize> = HashSet::from_iter(rules.iter().map(|(a,_)| *a));
    for (_,b) in rules {
        possible_smallest.remove(&b);
    }
    assert!(possible_smallest.len() == 1);
    return *possible_smallest.iter().next().unwrap();
}

fn parse_input(input: &String) -> (Vec<Rule>, Vec<Pages>) {
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rules: Vec<Rule> = parts[0].lines()
        .map(|line| line.split("|").collect::<Vec<&str>>())
        .map(|v| (v[0].parse().unwrap(), v[1].parse().unwrap()))
        .collect();
    let pages_arr: Vec<Pages> = parts[1].lines()
        .map(|line| line.split(",").map(|x| x.parse().unwrap()).collect())
        .collect();
    return (rules, pages_arr);
}