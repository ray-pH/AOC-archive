pub fn part1(input: &str) -> String {
    let (keys, locks) = parse_input(input);
    let mut not_overlapping_count = 0;
    for key in keys.iter() {
        for lock in locks.iter() {
            if !is_overlapping(key, lock) {
                not_overlapping_count += 1;
            }
        }
    }
    not_overlapping_count.to_string()
}

type Schematic = [u8;5];

fn is_overlapping(key: &Schematic, lock: &Schematic) -> bool {
    key.iter().zip(lock).any(|(k, l)| k+l > 5)
}


fn parse_input(input: &str) -> (Vec<Schematic>, Vec<Schematic>) {
    let mut keys: Vec<Schematic> = Vec::new();
    let mut locks: Vec<Schematic> = Vec::new();
    for str in input.split("\n\n") {
        let mut lines = str.lines();
        let is_key = lines.next().unwrap() == ".....";
        let mut schematic = [0; 5];
        for _ in 0..5 {
            let line = lines.next().unwrap();
            line.chars().enumerate().for_each(|(i, c)| {
                if c == '#' { schematic[i] += 1; }
            });
        }
        if is_key {
            keys.push(schematic);
        } else {
            locks.push(schematic);
        }
    }
    (keys, locks)
}