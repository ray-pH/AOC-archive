
pub fn part1(input: &str) -> String {
    let machines = parse_input(input);
    let tokens = machines.iter().map(get_token).collect::<Vec<isize>>();
    return tokens.iter().sum::<isize>().to_string();
}
pub fn part2(input: &str) -> String {
    let machines = parse_input(input).iter().map(correct_machine).collect::<Vec<_>>();
    let tokens = machines.iter().map(get_token).collect::<Vec<isize>>();
    return tokens.iter().sum::<isize>().to_string();
}

#[derive(Debug)]
struct MachineData {
    a: (isize, isize),
    b: (isize, isize),
    prize: (isize, isize),
}

// C1 [ax, ay] + C2 [bx, by] = [px, py]
// -> C1 ax + C2 bx = px
// -> C1 ay + C2 by = py
// -> (px - C2 bx) / ax = (py - C2 by) / ay
// -> px ay - C2 bx ay = py ax - C2 by ax
// -> C2 (by ax - bx ay) = py ax - px ay
// -> C2 = (py ax - px ay) / (by ax - bx ay)
// -> C1 = (px - C2 bx) / ax

fn get_token(machine: &MachineData) -> isize {
    if let Some((a, b)) = get_step(machine) {
        return a*3 + b;
    } else {
        return 0;
    }
}
fn get_step(machine: &MachineData) -> Option<(isize, isize)> {
    let (ax, ay) = machine.a;
    let (bx, by) = machine.b;
    let (px, py) = machine.prize;
    if (by * ax - bx * ay) == 0 {
        return None;
    }
    let c2 = (py * ax - px * ay) / (by * ax - bx * ay);
    let c1 = (px - c2 * bx) / ax;
    if c1 * ax + c2 * bx == px && c1 * ay + c2 * by == py {
        return Some((c1, c2));
    } else {
        return None;
    }
}

const CORRECTION: isize = 10000000000000;
fn correct_machine(machine: &MachineData) -> MachineData {
    MachineData {
        a: machine.a,
        b: machine.b,
        prize: (machine.prize.0 + CORRECTION, machine.prize.1 + CORRECTION),
    }
}

fn parse_input(input: &str) -> Vec<MachineData> {
    input.split("\n\n").map(parse_machine).collect()
}
fn parse_machine(input: &str) -> MachineData {
    let mut lines = input.lines();
    let a = parse_button(lines.next().unwrap());
    let b = parse_button(lines.next().unwrap());
    let prize = parse_prize(lines.next().unwrap());
    MachineData { a, b, prize }
}
fn parse_button(input: &str) -> (isize, isize) {
    let mut parts = input.split(',');
    let x = parts.next().unwrap().split('+').last().unwrap().parse::<isize>().unwrap();
    let y = parts.next().unwrap().split('+').last().unwrap().parse::<isize>().unwrap();
    (x, y)
}
fn parse_prize(input: &str) -> (isize, isize) {
    let mut parts = input.split(',');
    let x = parts.next().unwrap().split('=').last().unwrap().parse::<isize>().unwrap();
    let y = parts.next().unwrap().split('=').last().unwrap().parse::<isize>().unwrap();
    (x, y)
}
