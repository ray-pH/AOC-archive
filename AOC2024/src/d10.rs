use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> String {
    let mapdata = parse_input(input);
    let trailhead_pos = get_trailhead_pos(&mapdata);
    let tailmap = calculate_tailmap(&mapdata, &trailhead_pos);
    let trailhead_score: Vec<usize> = trailhead_pos.iter().map(|pos| tailmap.get(pos).unwrap().len()).collect();
    return trailhead_score.iter().sum::<usize>().to_string();
}
pub fn part2(input: &str) -> String {
    let mapdata = parse_input(input);
    let trailhead_pos = get_trailhead_pos(&mapdata);
    let scoremap = calculate_scoremap(&mapdata, &trailhead_pos);
    let trailhead_score: Vec<isize> = trailhead_pos.iter().map(|pos| scoremap[pos.0 as usize][pos.1 as usize]).collect();
    return trailhead_score.iter().sum::<isize>().to_string();
}

struct MapData {
    map: Map,
    row_count: isize,
    col_count: isize,
}
type Map = Vec<Vec<isize>>;
type TailMap = HashMap<Pos, HashSet<Pos>>;
type Pos = (isize, isize);

fn get_trailhead_pos(mapdata: &MapData) -> Vec<Pos> {
    let mut trailheads = Vec::new();
    for (i, row) in mapdata.map.iter().enumerate() {
        for (j, val) in row.iter().enumerate() {
            if *val == 0 { trailheads.push((i as isize, j as isize)); }
        }
    }
    return trailheads;
}

const DIR: [(isize, isize);4] = [(1,0),(0,1),(-1,0),(0,-1)];

fn calculate_tailmap(mapdata: &MapData, trailhead_pos: &[Pos]) -> TailMap {
    let mut tailmap = HashMap::new();
    trailhead_pos.iter().for_each(|pos| calculate_tailmap_from_pos(*pos, mapdata, &mut tailmap));
    return tailmap;
}

fn calculate_tailmap_from_pos(pos: Pos, mapdata: &MapData, tailmap: &mut TailMap) {
    let val = mapdata.map[pos.0 as usize][pos.1 as usize];
    if val == 9 {
        tailmap.insert(pos, HashSet::from([pos]));
        return;
    }
    
    tailmap.entry(pos).or_default();
    
    for (di, dj) in DIR {
        let npos = (pos.0 + di, pos.1 + dj);
        if is_pos_inside_map(&npos, mapdata) {
            let nval = mapdata.map[npos.0 as usize][npos.1 as usize];
            if nval == val + 1 {
                if !tailmap.contains_key(&npos) {
                    calculate_tailmap_from_pos(npos, mapdata, tailmap);
                }
                let ntail = tailmap.get(&npos).cloned().unwrap();
                tailmap.get_mut(&pos).unwrap().extend(ntail);
            }
        }
    }
}

fn calculate_scoremap(mapdata: &MapData, trailhead_pos: &[Pos]) -> Map {
    let mut scoremap = vec![vec![-1; mapdata.col_count as usize]; mapdata.row_count as usize];
    trailhead_pos.iter().for_each(|pos| calculate_scoremap_from_pos(*pos, mapdata, &mut scoremap));
    return scoremap;
}

fn calculate_scoremap_from_pos(pos: Pos, mapdata: &MapData, scoremap: &mut Map) {
    let val = mapdata.map[pos.0 as usize][pos.1 as usize];
    if val == 9 {
        scoremap[pos.0 as usize][pos.1 as usize] = 1;
        return;
    }
    
    let mut scores = 0;
    for (di, dj) in DIR {
        let npos = (pos.0 + di, pos.1 + dj);
        if is_pos_inside_map(&npos, mapdata) {
            let nval = mapdata.map[npos.0 as usize][npos.1 as usize];
            if nval == val + 1 {
                if scoremap[npos.0 as usize][npos.1 as usize] == -1 {
                    calculate_scoremap_from_pos(npos, mapdata, scoremap);
                }
                let nscore = scoremap[npos.0 as usize][npos.1 as usize];
                scores += nscore;
            }
        }
    }
    scoremap[pos.0 as usize][pos.1 as usize] = scores;
}

fn is_pos_inside_map(pos: &Pos, mapdata: &MapData) -> bool {
    (pos.0 >= 0) && (pos.0 < mapdata.row_count) &&
    (pos.1 >= 0) && (pos.1 < mapdata.col_count)
}

fn parse_input(input: &str) -> MapData {
    let map = input.lines()
        .map(|l| l.chars().map(|c| 
            c.to_digit(10).unwrap() as isize).collect())
        .collect::<Map>();
    let row_count = map.len() as isize;
    let col_count = map[0].len() as isize;
    MapData { map, row_count, col_count }
}