use crate::intcode::IntCodeComputer;

pub fn part1(input: &str) -> String {
    let mut comp = parse_input(input);
    comp.mem[1] = 12;
    comp.mem[2] = 2;
    comp.run();
    comp.mem[0].to_string()
}
pub fn part2(input: &str) -> String {
    let comp = parse_input(input);
    for noun in 0..100 {
        for verb in 0..100 {
            let mut mcomp = comp.clone();
            mcomp.mem[1] = noun;
            mcomp.mem[2] = verb;
            mcomp.run();
            if mcomp.mem[0] == 19690720 {
                return (100 * noun + verb).to_string();
            }
        }
    }
    return "Not Found".to_string();
}


fn parse_input(input: &str) -> IntCodeComputer {
    let mem = input.split(',').map(|s| s.parse().unwrap()).collect();
    IntCodeComputer { ptr: 0, mem, halt: false }
}