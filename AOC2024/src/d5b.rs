use std::{cmp::Ordering, collections::HashSet};

pub fn part1(input: &str) -> String {
    let (rules, pages_arr) = parse_input(input);
    let result: usize = pages_arr.iter()
        .filter(|pages| is_valid(&rules, pages))
        .map(get_middle_page)
        .sum();
    return result.to_string();
}

pub fn part2(input: &str) -> String {
    let (rules, pages_arr) = parse_input(input);
    let mut invalid_pages = pages_arr.iter()
        .filter(|pages| !is_valid(&rules, pages))
        .cloned().collect::<Vec<Pages>>();
    for pages in invalid_pages.iter_mut() {
        pages.sort_by(|a,b| if rules.contains(&(*a,*b)) { Ordering::Greater } else { Ordering::Less });
    }
    let result: usize = invalid_pages.iter().map(get_middle_page).sum();
    return result.to_string();
}

fn is_valid(rules: &Rules, pages: &Pages) -> bool {
    pages.windows(2).all(|w| !rules.contains(&(w[1], w[0])))
}

fn get_middle_page(pages: &Pages) -> usize {
    let middle = pages.len() / 2;
    return pages[middle];
}

type Rules = HashSet<(usize,usize)>;
type Pages = Vec<usize>;
fn parse_input(input: &str) -> (Rules, Vec<Pages>) {
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rules: Rules = HashSet::from_iter(
        parts[0].lines()
            .map(|line| line.split("|").collect::<Vec<&str>>())
            .map(|v| (v[0].parse().unwrap(), v[1].parse().unwrap()))
    );
    let pages_arr: Vec<Pages> = parts[1].lines()
        .map(|line| line.split(",").map(|x| x.parse().unwrap()).collect())
        .collect();
    return (rules, pages_arr);
}