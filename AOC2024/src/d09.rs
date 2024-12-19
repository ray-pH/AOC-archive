pub fn part1(input: &str) -> String {
    let diskmap = parse_input(input);
    let state = generate_initial_state(&diskmap);
    let (size, new_state) = process(state);
    return checksum(&new_state[0..=size]).to_string();
}

pub fn part2(input: &str) -> String {
    let diskmap = parse_input(input);
    let mut my_map = generate_map(&diskmap);
    process_my_map(&mut my_map);
    return checksum_my_map(&my_map).to_string();
}

fn generate_initial_state(diskmap: &[isize]) -> Vec<isize> {
    return diskmap.iter().enumerate()
        .flat_map(|(i, val)| {
            if i % 2 == 0 {
                return (0..*val).map(|_| i as isize/2).collect::<Vec<isize>>();
            } else {
                return (0..*val).map(|_| -1).collect::<Vec<isize>>();
            }
        })
        .collect();
}

fn process(mut state: Vec<isize>) -> (usize, Vec<isize>) {
    let mut l_ptr = 0;
    let mut r_ptr = state.len() - 1;
    loop {
        if l_ptr >= r_ptr {
            return (l_ptr, state);
        }
        if state[l_ptr] != -1 {
            l_ptr += 1;
            continue;
        } 
        if state[r_ptr] == -1 {
            r_ptr -= 1;
            continue;
        }
        else {
            state[l_ptr] = state[r_ptr];
            l_ptr += 1;
            r_ptr -= 1;
        }
    }
}

fn checksum(state: &[isize]) -> isize {
    state.iter().enumerate().map(|(i, val)| i as isize*val).sum()
}

#[derive(Debug)]
struct Chunk {
    index: usize,
    value: isize,
    size: usize,
}
#[derive(Debug)]
struct MyMap {
    filled: Vec<Chunk>,
    empty: Vec<Chunk>,
}

fn generate_map(diskmap: &[isize]) -> MyMap {
    let mut filled = Vec::new();
    let mut empty = Vec::new();
    let mut ptr = 0;
    diskmap.iter().enumerate().for_each(|(i, val)| {
        if i % 2 == 0 {
            filled.push(Chunk { value: i as isize/2, index: ptr, size: *val as usize });
        } else {
            empty.push(Chunk { value: -1, index: ptr, size: *val as usize });
        }
        ptr += *val as usize;
    });
    return MyMap { filled, empty };
}


fn insert_sorted(vec: &mut Vec<Chunk>, element: Chunk) {
    match vec.binary_search_by(|c: &Chunk| c.index.cmp(&element.index)) {
        Ok(pos) | Err(pos) => vec.insert(pos, element),
    }
}

fn process_my_map(map: &mut MyMap) {
    let mut ptr = map.filled.len() - 1;
    while ptr > 0 {
        let tomove_chunk = &map.filled[ptr];
        let empty_ptr = map.empty.iter().position(|ec| ec.size >= tomove_chunk.size && ec.index < tomove_chunk.index);
        if let Some(empty_ptr) = empty_ptr {
            let empty_index = map.empty[empty_ptr].index;
            if map.empty[empty_ptr].size == tomove_chunk.size {
                map.empty.remove(empty_ptr);
                let mut tomove = map.filled.remove(ptr);
                tomove.index = empty_index;
                insert_sorted(&mut map.filled, tomove);
            } else {
                let mut tomove = map.filled.remove(ptr);
                tomove.index = empty_index;
                map.empty[empty_ptr].size -= tomove.size;
                map.empty[empty_ptr].index += tomove.size;
                insert_sorted(&mut map.filled, tomove);
            }
        } else {
            ptr -= 1;
        }
    }
}

fn checksum_my_map(map: &MyMap) -> isize {
    return map.filled.iter()
        .map(|c| {
            let a = c.index as isize;
            let n = c.size as isize;
            let un = a + (n-1);
            (n * (a + un))/2 * c.value
        })
        .sum::<isize>();
}

fn parse_input(input: &str) -> Vec<isize> {
   input.lines().next().unwrap().chars().map(|c| c.to_digit(10).unwrap() as isize).collect()
}