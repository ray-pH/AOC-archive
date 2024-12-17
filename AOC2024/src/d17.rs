pub fn part1(input: &str) -> String {
    let mut c = parse_input(input);
    loop {
        if !step(&mut c) { break; }
    }
    return c.output_buffer.iter().map(|d| d.to_string()).collect::<Vec<_>>().join(",");
}

pub fn part2(input: &str) -> String {
    let mut c = parse_input(input);
    let target = c.program.clone();
    // let possible_init_a = find_init_a_given_target(&target, &mut |x| quick_eval(x));
    let possible_init_a = find_init_a_given_target(&target, &mut move |x| reg_a_eval_computer(x, &mut c));
    let reg_a = *possible_init_a.iter().min().unwrap();
    return reg_a.to_string();
}

#[derive(Clone)]
struct Computer {
    ptr: usize,
    reg_a: usize,
    reg_b: usize,
    reg_c: usize,
    program: Vec<usize>,
    output_buffer: Vec<usize>,
}
impl Computer {
    fn combo(&self, val: usize) -> usize {
        match val {
            0..=3 => val,
            4 => self.reg_a,
            5 => self.reg_b,
            6 => self.reg_c,
            _ => panic!("invalid combo operand"),
        }
    }
    fn reset(&mut self) {
        self.ptr = 0;
        self.reg_a = 0;
        self.reg_b = 0;
        self.reg_c = 0;
        self.output_buffer.clear();
    }
}

fn step(c: &mut Computer) -> bool {
    let opcode = c.program[c.ptr];
    let val = c.program[c.ptr + 1];
    match opcode {
        0 => {
            // The adv instruction (opcode 0) 
            // performs division. The numerator is the value in the A register. 
            // The denominator is found by raising 2 to the power of the instruction's combo operand. 
            // (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
            // The result of the division operation is truncated to an integer and then written to the A register.
            c.reg_a >>= c.combo(val);
        }
        1 => {
            // The bxl instruction (opcode 1)
            // calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
            c.reg_b ^= val;
        }
        2 => {
            // The bst instruction (opcode 2)
            // calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
            c.reg_b = c.combo(val) & 0b111;
        }
        3 => {
            // The jnz instruction (opcode 3)
            // does nothing if the A register is 0. 
            // However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; 
            // if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
            if c.reg_a != 0 {
                c.ptr = val - 2;
            }
        }
        4 => {
            // The bxc instruction (opcode 4)
            // calculates the bitwise XOR of register B and register C, then stores the result in register B. 
            // (For legacy reasons, this instruction reads an operand but ignores it.)
            c.reg_b ^= c.reg_c;
        }
        5 => {
            // The out instruction (opcode 5)
            // calculates the value of its combo operand modulo 8, then outputs that value. 
            // (If a program outputs multiple values, they are separated by commas.)
            c.output_buffer.push(c.combo(val) & 0b111);
        }
        6 => {
            // The bdv instruction (opcode 6)
            // works exactly like the adv instruction except that the result is stored in the B register. 
            // (The numerator is still read from the A register.)
            c.reg_b = c.reg_a >> c.combo(val);
        }
        7 => {
            // The cdv instruction (opcode 7)
            // works exactly like the adv instruction except that the result is stored in the C register. 
            // (The numerator is still read from the A register.)
            c.reg_c = c.reg_a >> c.combo(val);
        }
        _ => panic!("Invalid opcode"),
    }
    c.ptr += 2;
    c.ptr < c.program.len()
}

type StepRegEvalFn = dyn FnMut(usize) -> usize;
fn find_init_a_given_target(target: &[usize], step_a_eval: &mut StepRegEvalFn) -> Vec<usize> {
    let mut curr_as = vec![0];
    for x in target.iter().rev() {
        curr_as = curr_as.iter().flat_map(|curr_a| find_possible_prev_a(*curr_a, *x, step_a_eval)).collect();
    }
    return curr_as;
}
fn find_possible_prev_a(curr_a: usize, target_digit: usize, step_a_eval: &mut StepRegEvalFn) -> Vec<usize> {
    let lower = curr_a * 8;
    let upper = (curr_a + 1) * 8;
    (lower..upper).filter(|i| step_a_eval(*i) == target_digit).collect()
}
fn reg_a_eval_computer(reg_a: usize, c: &mut Computer) -> usize {
    c.reset();
    c.reg_a = reg_a;
    loop {
        step(c);
        if c.output_buffer.len() == 1 { break; }
    }
    c.output_buffer[0]
}

// compiled manually
#[allow(dead_code)]
fn quick_eval(a: usize) -> usize {
    let b = a & 0b111;
    let b = b ^ 0b001;
    let c = a >> b;
    // let a = a >> 0b011;
    let b = b ^ c;
    let b = b ^ 0b110;
    #[allow(clippy::let_and_return)]
    let out = b & 0b111;
    out
}

fn parse_input(input: &str) -> Computer {
    let mut lines = input.lines();
    let reg_a = lines.next().unwrap().split(": ").last().unwrap().parse::<usize>().unwrap();
    let reg_b = lines.next().unwrap().split(": ").last().unwrap().parse::<usize>().unwrap();
    let reg_c = lines.next().unwrap().split(": ").last().unwrap().parse::<usize>().unwrap();
    lines.next();
    let program = lines.next().unwrap().split(": ").last().unwrap()
        .split(',').map(|d| d.parse::<usize>().unwrap()).collect();
    Computer {
        ptr: 0,
        reg_a, reg_b, reg_c, program,
        output_buffer: Vec::new(),
    }
}