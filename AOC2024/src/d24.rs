use std::collections::HashMap;

const INPUT_BITS: usize = 45;
pub fn part1(input: &str) -> String {
    let (mut values, wiring) = parse_input(input);
    let mut zs: Vec<Node> = wiring.keys().filter(|c| c[0] == 'z').cloned().collect();
    zs.sort();
    zs.iter().for_each(|z| {
        solve(z, &mut values, &wiring);
    });
    let zs_value: Vec<bool> = zs.iter().map(|z| values[z]).collect();
    zs_value.iter()
        .enumerate().filter(|(_, v)| **v)
        .map(|(i, _)| 1 << i)
        .sum::<usize>().to_string()
}
pub fn part2(input: &str) -> String {
    let (_, wiring) = parse_input(input);
    let parents = generate_parents_map(&wiring);
    let mut wrong_wiring: Vec<Node> = Vec::new();
    for i in 1..INPUT_BITS-1 {
        let swap_vec = get_possible_swap(&wiring, &parents, i).unwrap();
        wrong_wiring.extend(&swap_vec);
    }
    wrong_wiring.sort();
    wrong_wiring.iter().map(|n| n.iter().collect::<String>()).collect::<Vec<String>>().join(",")
}

fn get_possible_swap(wiring: &HashMap<Node, WiringInput>, parents: &HashMap<Node, Vec<Node>>, n: usize) -> Option<Vec<Node>> {
    let mut result: Vec<Node> = Vec::new();
    let x = usize_to_node('x', n);
    let y = usize_to_node('y', n);
    let xpar = parents.get(&x).unwrap();
    let ypar = parents.get(&y).unwrap();
    if !is_vec_set_same(xpar, ypar) {
        todo!();
    }
    let xor_parent = xpar.iter().find(|p| wiring[*p].gate == Gate::Xor);
    let and_parent = xpar.iter().find(|p| wiring[*p].gate == Gate::And);
    if let Some(xor_parent) = xor_parent {
        if and_parent.is_none() {
            // todo!();
        }
        if xor_parent[0] == 'z' {
            result.push(*xor_parent);
        } else {
            let mut xor_parent_valid = true;
            let xor_parent2 = parents.get(xor_parent).unwrap().iter().find(|p| wiring[*p].gate == Gate::Xor);
            if let Some(xor_parent2) = xor_parent2 {
                if xor_parent2[0] != 'z' {
                    result.push(*xor_parent2);
                }
            }
            if xor_parent2.is_none() {
                xor_parent_valid = false;
            }
            let and_parent2 = parents.get(xor_parent).unwrap().iter().find(|p| wiring[*p].gate == Gate::And);
            if let Some(and_parent2) = and_parent2 {
                if and_parent2[0] == 'z' {
                    result.push(*and_parent2);
                }
            }
            if and_parent2.is_none() {
                xor_parent_valid = false;
            }
            if !xor_parent_valid {
                result.push(*xor_parent);
            }
        }
    }
    if let Some(and_parent) = and_parent {
        if xor_parent.is_none() {
            // todo!();
        }
        if and_parent[0] == 'z' {
            result.push(*and_parent);
        } else {
            let mut and_parent_valid = true;
            let or_parent2 = parents.get(and_parent).unwrap().iter().find(|p| wiring[*p].gate == Gate::Or);
            if let Some(or_parent2) = or_parent2 {
                if or_parent2[0] == 'z' {
                    result.push(*or_parent2);
                }
            }
            if or_parent2.is_none() {
                and_parent_valid = false;
            }
            if !and_parent_valid {
                result.push(*and_parent);
            }
        }
    }
    return Some(result);
}

fn is_vec_set_same<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    a.iter().all(|x| b.contains(x))
}

fn usize_to_node(prefix: char, n: usize) -> Node {
    let i0 = char::from_digit(n as u32 % 10, 10).unwrap();
    let i1 = char::from_digit(n as u32 / 10, 10).unwrap();
    [prefix, i1, i0]
}

fn generate_parents_map(wiring: &HashMap<Node, WiringInput>) -> HashMap<Node, Vec<Node>> {
    let mut parents: HashMap<Node, Vec<Node>> = HashMap::new();
    for (node, wiring) in wiring.iter() {
        parents.entry(wiring.inp_a).or_default().push(*node);
        parents.entry(wiring.inp_b).or_default().push(*node);
    }
    parents
}

#[derive(PartialEq, Eq)]
enum Gate { And, Xor, Or }
type Node = [char; 3];
struct WiringInput { inp_a: Node, inp_b: Node, gate: Gate }

fn solve(target: &Node, values: &mut HashMap<Node, bool>, wiring: &HashMap<Node, WiringInput>) -> bool {
    if let Some(value) = values.get(target) {
        return *value;
    } else {
        let w = wiring.get(target).unwrap();
        let val_a = solve(&w.inp_a, values, wiring);
        let val_b = solve(&w.inp_b, values, wiring);
        let val_target = eval_wiring(val_a, val_b, &w.gate);
        values.insert(*target, val_target);
        return val_target;
    }
}

fn eval_wiring(val_a: bool, val_b: bool, gate: &Gate) -> bool {
    match gate {
        Gate::And => val_a && val_b,
        Gate::Xor => val_a ^ val_b,
        Gate::Or => val_a || val_b,
    }
}

fn parse_input(input: &str) -> (HashMap<Node, bool>, HashMap<Node, WiringInput>) {
    let mut iter = input.split("\n\n");
    (parse_initial_value(iter.next().unwrap()), parse_wiring(iter.next().unwrap()))
}
fn parse_initial_value(input: &str) -> HashMap<Node, bool> {
    HashMap::from_iter(input.lines().map(|line|{
        let mut iter = line.split(": ");
        let name_vec: Vec<char> = iter.next().unwrap().chars().collect();
        let name: Node = [name_vec[0], name_vec[1], name_vec[2]];
        let value_num: u8 = iter.next().unwrap().parse().unwrap();
        (name, value_num == 1)
    }))
}
fn parse_wiring(input: &str) -> HashMap<Node, WiringInput> {
    // x00 AND y00 -> z00
    HashMap::from_iter(input.lines().map(|line|{
        let mut iter = line.split(" ");
        let name_vec: Vec<char> = iter.next().unwrap().chars().collect();
        let inp_a: Node = [name_vec[0], name_vec[1], name_vec[2]];
        let gate = gate_from_str(iter.next().unwrap());
        let name_vec: Vec<char> = iter.next().unwrap().chars().collect();
        let inp_b: Node = [name_vec[0], name_vec[1], name_vec[2]];
        iter.next();
        let name_vec: Vec<char> = iter.next().unwrap().chars().collect();
        let out: Node = [name_vec[0], name_vec[1], name_vec[2]];
        (out, WiringInput { inp_a, inp_b, gate })
    }))
}
fn gate_from_str(s: &str) -> Gate {
    match s {
        "AND" => Gate::And,
        "XOR" => Gate::Xor,
        "OR" => Gate::Or,
        _ => panic!("Invalid gate")
    }
}

// fn generate_dotfile(wiring: &HashMap<Node, WiringInput>) {
//     let mut dotstr: String = "".to_string();
//     dotstr.push_str("digraph G {\n");
//     dotstr.push_str("    rankdir=BT;\n");
//     dotstr.push_str("    ranksep=1.5;\n");
//     for i in 0..INPUT_BITS {
//         let i0 = char::from_digit(i as u32 % 10, 10).unwrap();
//         let i1 = char::from_digit(i as u32 / 10, 10).unwrap();
//         let j = i+1;
//         let j0 = char::from_digit(j as u32 % 10, 10).unwrap();
//         let j1 = char::from_digit(j as u32 / 10, 10).unwrap();
//         dotstr.push_str(&format!("   subgraph rank{i1}{i0} {{\n"));
//         dotstr.push_str("        rank=same;\n");
//         // dotstr.push_str(&format!("        x{i1}{i0}; y{i1}{i0}; z{j1}{j0};\n"));
//         dotstr.push_str(&format!("        x{i1}{i0}; y{i1}{i0}\n"));
//         dotstr.push_str(&format!("   }}\n"));
//     }
//     for (node, wiring) in wiring.iter() {
//         let shape = match wiring.gate {
//             Gate::AND => "box",
//             Gate::XOR => "diamond",
//             Gate::OR => "oval",
//         };
//         let out_name: String = node.iter().collect();
//         let a_name: String = wiring.inp_a.iter().collect();
//         let b_name: String = wiring.inp_b.iter().collect();
//         if node[0] == 'z' {
//             dotstr.push_str(&format!("   {out_name} [shape={shape}, style=filled, fillcolor=cyan]\n"));
//         } else {
//             dotstr.push_str(&format!("   {out_name} [shape={shape}]\n"));
//         }
//         dotstr.push_str(&format!("   {out_name} -> {a_name}\n"));
//         dotstr.push_str(&format!("   {out_name} -> {b_name}\n"));
//     }
//     dotstr.push_str("}");
    
//     let filename = "dot/d24.dot";
//     let mut file = std::fs::File::create(filename).unwrap();
//     file.write_all(dotstr.as_bytes()).unwrap();
// }