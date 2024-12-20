use std::{collections::{BinaryHeap, HashMap}, ops::{Add, Div, Mul, Rem, Sub}};

pub struct Rational<T> {
    pub numerator: T,
    pub denominator: T,
}

impl<T> Rational<T>
where
    T: Mul<Output = T> + Add<Output = T> + Sub<Output = T> + Rem<Output = T> + Div<Output = T> + Default + PartialEq + Copy,
{
    pub fn new(numerator: T, denominator: T) -> Self {
        Self {
            numerator,
            denominator,
        }
    }
    pub fn is_integer(&self) -> bool {
        self.numerator % self.denominator == T::default()
    }
    pub fn get_integer_value(&self) -> T {
        self.numerator / self.denominator
    }
    pub fn add(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.denominator + other.numerator * self.denominator,
            denominator: self.denominator * other.denominator,
        }
    }
    pub fn sub(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.denominator - other.numerator * self.denominator,
            denominator: self.denominator * other.denominator,
        }
    }
    #[allow(dead_code)]
    pub fn mul(&self, other: &Self) -> Self {
        Self {
            numerator: self.numerator * other.numerator,
            denominator: self.denominator * other.denominator,
        }
    }
    pub fn mul_scalar(&self, scalar: T) -> Self {
        Self {
            numerator: self.numerator * scalar,
            denominator: self.denominator,
        }
    }
}

pub fn positive_mod(a: isize, b: isize) -> isize {
    (a % b + b) % b
}

pub fn starts_with<I>(mut iter: I, prefix: I) -> bool
where
    I: Iterator,
    I::Item: PartialEq,
{
    for a in prefix {
        match iter.next() {
            Some(b) if a == b => continue,
            _ => return false,
        }
    }
    return true;
}


// T is the coordinate type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AStarNode<T> {
    pos: T,
    cost: isize,
    est_cost: isize,
}
impl<T: Eq> Ord for AStarNode<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.est_cost.cmp(&self.est_cost)
        .then_with(|| self.cost.cmp(&other.cost))
    }
}
impl<T: Eq> PartialOrd for AStarNode<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub trait AStarGraph<T> {
    fn get_neighbors(&self, node: &T) -> Vec<(T, isize)>;
    fn get_heutistic(&self, node: &T) -> isize;
    fn get_start(&self) -> T;
    fn is_goal(&self, node: &T) -> bool;
}

pub fn a_star<T>(graph: &impl AStarGraph<T>) -> Option<(Vec<T>, isize)>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug,
{
    let mut open_set: BinaryHeap<AStarNode<T>> = BinaryHeap::new();
    let mut came_from: HashMap<T, T> = HashMap::new();
    let mut g_score: HashMap<T, isize> = HashMap::new();
    
    let start = graph.get_start();
    open_set.push(AStarNode {
        pos: start.clone(),
        cost: 0,
        est_cost: graph.get_heutistic(&start),
    });
    g_score.insert(start, 0);
    
    while let Some(current) = open_set.pop() {
        if graph.is_goal(&current.pos) {
            // reconstruct the path
            let mut path = vec![current.pos];
            while let Some(prev) = came_from.get(&path[0]) {
                path.insert(0, prev.clone());
            }
            return Some((path, current.cost));
        }
        for (neigh, cost) in graph.get_neighbors(&current.pos) {
            let tentative_g_score = g_score[&current.pos] + cost;
            if tentative_g_score < *g_score.get(&neigh).unwrap_or(&isize::MAX) {
                came_from.insert(neigh.clone(), current.pos.clone());
                g_score.insert(neigh.clone(), tentative_g_score);
                open_set.push(AStarNode {
                    pos: neigh.clone(),
                    cost: tentative_g_score,
                    est_cost: tentative_g_score + graph.get_heutistic(&neigh),
                });
            }
        }
    }
    return None;
}



// T is the coordinate type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AStarNode2<T> {
    pos: T,
    cost: isize,
    est_cost: isize,
}
impl<T: Eq> Ord for AStarNode2<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.est_cost.cmp(&self.est_cost)
        .then_with(|| self.cost.cmp(&other.cost))
    }
}
impl<T: Eq> PartialOrd for AStarNode2<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub trait AStarGraph2<T> {
    fn get_neighbors(&self, node: &T) -> &Vec<(T, isize)>;
    fn get_heutistic(&self, node: &T) -> isize;
    fn get_start(&self) -> T;
    fn is_goal(&self, node: &T) -> bool;
}

pub fn a_star2<T>(graph: &impl AStarGraph2<T>) -> Option<(Vec<T>, isize)>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug,
{
    let mut open_set: BinaryHeap<AStarNode2<T>> = BinaryHeap::new();
    let mut came_from: HashMap<T, T> = HashMap::new();
    let mut g_score: HashMap<T, isize> = HashMap::new();
    
    let start = graph.get_start();
    open_set.push(AStarNode2 {
        pos: start.clone(),
        cost: 0,
        est_cost: graph.get_heutistic(&start),
    });
    g_score.insert(start, 0);
    
    while let Some(current) = open_set.pop() {
        if graph.is_goal(&current.pos) {
            // reconstruct the path
            let mut path = vec![current.pos];
            while let Some(prev) = came_from.get(&path[0]) {
                path.insert(0, prev.clone());
            }
            return Some((path, current.cost));
        }
        for (neigh, cost) in graph.get_neighbors(&current.pos) {
            let tentative_g_score = g_score[&current.pos] + cost;
            if tentative_g_score < *g_score.get(&neigh).unwrap_or(&isize::MAX) {
                came_from.insert(neigh.clone(), current.pos.clone());
                g_score.insert(neigh.clone(), tentative_g_score);
                open_set.push(AStarNode2 {
                    pos: neigh.clone(),
                    cost: tentative_g_score,
                    est_cost: tentative_g_score + graph.get_heutistic(&neigh),
                });
            }
        }
    }
    return None;
}