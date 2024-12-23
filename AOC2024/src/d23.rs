use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let three_pairs = generate_t_three_pairs(&map);
    three_pairs.len().to_string()
}
pub fn part2(input: &str) -> String {
    let map = parse_input(input);
    let all_nodes: Vec<Computer> = map.keys().cloned().collect();
    let max_clique = bron_kerbosch(&map, Vec::new(), all_nodes, Vec::new());
    let mut max_clique_vec: Vec<String> = max_clique.iter().map(computer_to_string).collect();
    max_clique_vec.sort();
    max_clique_vec.join(",")
}

fn generate_t_three_pairs(map: &ComputerMap) -> HashSet<(&Computer, &Computer, &Computer)> {
    let mut set: HashSet<(&Computer, &Computer, &Computer)> = HashSet::new();
    let mut visited: HashSet<(&Computer, &Computer)> = HashSet::new();
    map.keys().filter(|ca| ca.0 == 't').for_each(|ca| {
        map[ca].iter().for_each(|cb| {
            if !visited.contains(&(ca, cb)) {
                map[ca].intersection(&map[cb]).for_each(|cc| {
                    let mut key = [ca, cb, cc];
                    key.sort();
                    set.insert((key[0], key[1], key[2]));
                });
                visited.insert((ca, cb));
            }
        });
    });
    set
}

fn bron_kerbosch(graph: &ComputerMap, r: ComputerVec, p: ComputerVec, x: ComputerVec) -> ComputerVec {
    if p.is_empty() && x.is_empty() {
        return r.to_vec();
    }
    let mut x = x.clone();
    let mut mut_p = p.clone();
    let mut max_clique: ComputerVec = Vec::new();
    for v in p.iter().rev() {
        let new_r: Vec<_> = r.iter().chain([v]).cloned().collect();
        
        if let Some(neighbors) = graph.get(v) {
            let new_p = mut_p.iter().filter(|n| neighbors.contains(n)).cloned().collect();
            let new_x = x.iter().filter(|n| neighbors.contains(n)).cloned().collect();
            let clique = bron_kerbosch(graph, new_r, new_p, new_x);
            if clique.len() > max_clique.len() {
                max_clique = clique;
            }
        }
        mut_p.pop();
        x.push(*v);
    }
    return max_clique;
}

fn computer_to_string(computer: &Computer) -> String {
    format!("{}{}", computer.0, computer.1)
}

type Computer = (char, char);
type ComputerMap = HashMap<Computer, HashSet<Computer>>;
type ComputerVec = Vec<Computer>;
fn parse_input(input: &str) -> ComputerMap {
    let mut computers: ComputerMap = HashMap::new();
    input.lines().for_each(|line| {
        let mut chars = line.chars();
        let comp_a = (chars.next().unwrap(), chars.next().unwrap());
        chars.next();
        let comp_b = (chars.next().unwrap(), chars.next().unwrap());
        computers.entry(comp_a).or_default().insert(comp_b);
        computers.entry(comp_b).or_default().insert(comp_a);
    });
    computers
}

// fn generate_dotfile(input: &str) {
//     let mut dotstr: String = "".to_string();
//     dotstr.push_str("Graph {\n");
//     input.lines().for_each(|line| {
//         let mut iter = line.split('-');
//         let comp_a = iter.next().unwrap();
//         let comp_b = iter.next().unwrap();
//         let line = format!("    {comp_a} -- {comp_b}\n");
//         dotstr.push_str(&line);
//     });
//     dotstr.push_str("}");
    
//     let filename = "dot/d23.dot";
//     let mut file = std::fs::File::create(filename).unwrap();
//     file.write_all(dotstr.as_bytes()).unwrap();
// }