#![allow(dead_code)]
pub fn part1(input: &str) -> String {
    let stones = parse_input(input);
    let mut stones_ll = FauxLinkedListContainer::from_slice(&stones);
    for _ in 0..25 {
        blink_linked_list(&mut stones_ll);
    }
    return stones_ll.arena.len().to_string();
}

pub fn part2(input: &str) -> String {
    let stones = parse_input(input);
    let mut stones_ll = FauxLinkedListContainer::from_slice(&stones);
    for i in 0..75 {
        dbg!(i, stones_ll.arena.len());
        blink_linked_list(&mut stones_ll);
    }
    return stones_ll.arena.len().to_string();
}

fn blink_linked_list(stones_ll: &mut FauxLinkedListContainer) {
    stones_ll.reset_ptr();
    loop {
        let val = stones_ll.get_val();
        if val == 0 {
            stones_ll.replace(1);
        } else if let Some((a,b)) = split_even_digit(val) {
            stones_ll.replace(a);
            stones_ll.insert_next(b);
            stones_ll.next();
        } else {
            stones_ll.replace(val * 2024);
        };
        if !stones_ll.is_next_exist() {
            break;
        }
        stones_ll.next();
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

// struct 

struct FauxLinkedList {
    pub value: u64,
    pub prev_index: Option<usize>,
    pub next_index: Option<usize>,
}
struct FauxLinkedListContainer {
    pub ptr: usize,
    pub arena: Vec<FauxLinkedList>,
}
impl FauxLinkedListContainer {
    pub fn reset_ptr(&mut self) {
        self.ptr = 0;
    }
    pub fn next(&mut self) {
        self.ptr = self.arena[self.ptr].next_index.unwrap();
    }
    pub fn is_next_exist(&self) -> bool {
        self.arena[self.ptr].next_index.is_some()
    }
    pub fn from_slice(vec: &[u64]) -> Self {
        let arena = vec.iter().enumerate().map(|(i, v)| {
            FauxLinkedList {
                value: *v,
                prev_index: if i == 0 { None } else { Some(i-1) },
                next_index: if i == vec.len()-1 { None } else { Some(i+1) },
            }
        }).collect();
        FauxLinkedListContainer { ptr: 0, arena }
    }
    pub fn replace(&mut self, val: u64){
        self.arena[self.ptr].value = val;
    }
    pub fn get_val(&self) -> u64 {
        self.arena[self.ptr].value
    }
    pub fn insert_next(&mut self, val: u64) {
        let curr_len = self.arena.len();
        let new_index = curr_len;
        if let Some(next_index) = self.arena[self.ptr].next_index {
            self.arena[next_index].prev_index = Some(new_index);
        }
        let new_element = FauxLinkedList {
            value: val,
            prev_index: Some(self.ptr),
            next_index: self.arena[self.ptr].next_index,
        };
        self.arena[self.ptr].next_index = Some(new_index);
        self.arena.push(new_element);
    }
}