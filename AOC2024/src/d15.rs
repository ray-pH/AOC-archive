use std::collections::HashSet;

pub fn part1(input: &str) -> String {
    let (mut mapdata, dirs) = parse_input(input);
    for dir in dirs {
        let player_pos = mapdata.player;
        let (_, npos) = try_move(&mut mapdata, player_pos, &dir);
        mapdata.player = npos;
        // println!("---------------------");
        // print_map(&mapdata);
    }
    return get_score(&mapdata).to_string();
}
pub fn part2(input: &str) -> String {
    let (init_mapdata, dirs) = parse_input(input);
    let mut mapdata = mod_map(init_mapdata);
    let mut can_move_cache = HashSet::new();
    for dir in dirs.iter() {
        can_move_cache.clear();
        let player_pos = mapdata.player;
        let (_, npos) = try_move2(&mut mapdata, player_pos, dir, &mut can_move_cache);
        mapdata.player = npos;
        // println!("---------------------");
        // print_map(&mapdata);
    }
    return get_score(&mapdata).to_string();
}

// fn print_map(map: &MapData) {
//     for row in map.map.iter() {
//         let line = row.iter().map(|c| c.to_string()).collect::<Vec<_>>().join("");
//         println!("{}", line);
//     }
// }

type Pos = (i32,i32);
enum Dir { Up, Down, Left, Right }
impl Dir {
    pub fn to_tuple(&self) -> Pos {
        match self {
            Dir::Up => (-1, 0),
            Dir::Down => (1, 0),
            Dir::Left => (0, -1),
            Dir::Right => (0, 1),
        }
    }
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '^' => Some(Dir::Up),
            'v' => Some(Dir::Down),
            '>' => Some(Dir::Right),
            '<' => Some(Dir::Left),
            _ => None
        }
    }
}

struct MapData {
    pub map: Vec<Vec<char>>,
    pub player: Pos,
    pub row_count: i32,
    pub col_count: i32,
}

fn try_move(map: &mut MapData, from: Pos, dir: &Dir) -> (bool, Pos) {
    let diff = dir.to_tuple();
    let to = (from.0 + diff.0, from.1 + diff.1);
    let targetc = map.map[to.0 as usize][to.1 as usize];
    match targetc {
        '#' =>  (false, from),
        '.' => {
            map.map[to.0 as usize][to.1 as usize] = map.map[from.0 as usize][from.1 as usize];
            map.map[from.0 as usize][from.1 as usize] = '.';
            (true, to)
        }
        'O' => {
            let (success, _) = try_move(map, to, dir);
            if success {
                map.map[to.0 as usize][to.1 as usize] = map.map[from.0 as usize][from.1 as usize];
                map.map[from.0 as usize][from.1 as usize] = '.';
                (true, to)
            } else {
                (false, from)
            }
        }
        _ => (false, from)
    }
}

fn get_score(map: &MapData) -> usize {
    let mut score = 0;
    for (row, line) in map.map.iter().enumerate() {
        for (col, c) in line.iter().enumerate() {
            if *c == 'O' || *c == '[' {
                score += 100*row + col;
            }
        }
    }
    return score;
}

fn can_move_to2(map: &MapData, pos: Pos, dir: &Dir, can_move_cache: &mut HashSet<Pos>) -> bool {
    if can_move_cache.contains(&pos) {
        return true;
    }
    let c = map.map[pos.0 as usize][pos.1 as usize];
    match c {
        '.' => true,
        '[' => {
            let diff = dir.to_tuple();
            let to = (pos.0 + diff.0, pos.1 + diff.1);
            let pairto = (to.0, to.1 + 1);
            match dir {
                Dir::Up | Dir::Down => {
                    can_move_to2(map, to, dir, can_move_cache) && 
                    can_move_to2(map, pairto, dir, can_move_cache)
                }
                Dir::Left => {
                    can_move_to2(map, to, dir, can_move_cache)
                }
                Dir::Right => {
                    can_move_to2(map, pairto, dir, can_move_cache)
                }
            }
        }
        ']' => {
            let pairpos = (pos.0, pos.1 - 1);
            can_move_to2(map, pairpos, dir, can_move_cache)
        }
        _ => false
    }
}

fn try_move2(map: &mut MapData, from: Pos, dir: &Dir, can_move_cache: &mut HashSet<Pos>) -> (bool, Pos) {
    let diff = dir.to_tuple();
    let to = (from.0 + diff.0, from.1 + diff.1);
    let c = map.map[from.0 as usize][from.1 as usize];
    match c {
        '@' => {
            // player
            if can_move_to2(map, to, dir, can_move_cache){
                try_move2(map, to, dir, can_move_cache);
                map.map[to.0 as usize][to.1 as usize] = '@';
                map.map[from.0 as usize][from.1 as usize] = '.';
                (true, to)
            } else {
                (false, from)
            }
        }
        ']' => {
            let pairpos = (from.0, from.1 - 1);
            try_move2(map, pairpos, dir, can_move_cache)
        }
        '[' => {
            let pairfrom = (from.0, from.1 + 1);
            let pairto = (to.0, to.1 + 1);
            match dir {
                Dir::Up | Dir::Down => {
                    if can_move_to2(map, to, dir, can_move_cache) && can_move_to2(map, pairto, dir, can_move_cache) {
                        try_move2(map, to, dir, can_move_cache);
                        try_move2(map, pairto, dir, can_move_cache);
                        map.map[to.0 as usize][to.1 as usize] = '[';
                        map.map[from.0 as usize][from.1 as usize] = '.';
                        map.map[pairto.0 as usize][pairto.1 as usize] = ']';
                        map.map[pairfrom.0 as usize][pairfrom.1 as usize] = '.';
                        (true, to)
                    } else {
                        (false, from)
                    }
                }
                Dir::Left => {
                    if can_move_to2(map, to, dir, can_move_cache) {
                        try_move2(map, to, dir, can_move_cache);
                        map.map[to.0 as usize][to.1 as usize] = '[';
                        map.map[from.0 as usize][from.1 as usize] = '.';
                        map.map[pairto.0 as usize][pairto.1 as usize] = ']';
                        map.map[pairfrom.0 as usize][pairfrom.1 as usize] = '.';
                        (true, to)
                    } else {
                        (false, from)
                    }
                }
                Dir::Right => {
                    if can_move_to2(map, pairto, dir, can_move_cache) {
                        try_move2(map, pairto, dir, can_move_cache);
                        map.map[pairto.0 as usize][pairto.1 as usize] = ']';
                        map.map[pairfrom.0 as usize][pairfrom.1 as usize] = '.';
                        map.map[to.0 as usize][to.1 as usize] = '[';
                        map.map[from.0 as usize][from.1 as usize] = '.';
                        (true, to)
                    } else {
                        (false, from)
                    }
                }
            }
        }
        _ => (false, from)
    }
}

fn mod_map(mapdata: MapData) -> MapData {
    let map = mapdata.map
        .iter().map(|row| row.iter().flat_map(|c| {
            match c {
                '.' => vec!['.', '.'],
                'O' => vec!['[', ']'],
                '#' => vec!['#', '#'],
                '@' => vec!['@', '.'],
                _   => vec![*c,*c],
            }
        }).collect()).collect::<Vec<Vec<char>>>();
    let p = mapdata.player;
    MapData {
        map,
        player: (p.0, p.1 * 2),
        row_count: mapdata.row_count,
        col_count: mapdata.col_count * 2,
    }
}

fn parse_input(input: &str) -> (MapData, Vec<Dir>) {
    let mut parts = input.split("\n\n");
    let map = parts.next().unwrap().lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let row_count = map.len() as i32;
    let col_count = map[0].len() as i32;
    let mut player = (0, 0);
    for (row, line) in map.iter().enumerate() {
        for (col, c) in line.iter().enumerate() {
            if *c == '@' {
                player = (row as i32, col as i32);
            }
        }
    }
    let dirs = parts.next().unwrap().lines()
        .flat_map(|line| line.chars().map(|c| Dir::from_char(c).unwrap()).collect::<Vec<_>>())
        .collect();
    (MapData { map, player, row_count, col_count}, dirs)
}