pub fn part1(input: &str) -> String {
    let map = parse_input(input);
    let mut visited = vec![vec![false; map.col_count]; map.row_count];
    let mut regions: Vec<Region> = vec![];
    for i in 0..map.row_count {
        for j in 0..map.col_count {
            if !visited[i][j] {
                let region = get_region(&(i as isize,j as isize), &map, &mut visited);
                regions.push(region);
            }
        }
    }
    let prices = regions.iter().map(|r| r.area * r.perimeter);
    return prices.sum::<usize>().to_string();
}
pub fn part2(input: &str) -> String {
    let map = parse_input(input);
    let mut visited = vec![vec![false; map.col_count]; map.row_count];
    let mut regions: Vec<Region2> = vec![];
    for i in 0..map.row_count {
        for j in 0..map.col_count {
            if !visited[i][j] {
                let region = get_region2(&(i as isize,j as isize), &map, &mut visited);
                regions.push(region);
            }
        }
    }
    let regions1 = regions.into_iter().map(count_region2_side).collect::<Vec<Region>>();
    let prices = regions1.iter().map(|r| r.area * r.perimeter);
    return prices.sum::<usize>().to_string();
}

type Pos = (isize, isize);
struct MapData {
    pub map: Vec<Vec<char>>,
    pub row_count: usize,
    pub col_count: usize,
}
struct Region {
    pub char: char,
    pub area: usize,
    pub perimeter: usize,
}

fn is_pos_inside((row,col): &Pos, map: &MapData) -> bool {
    *row >= 0 && *row < map.row_count as isize && 
    *col >= 0 && *col < map.col_count as isize
}

fn get_region(pos: &Pos, map: &MapData, visited: &mut Vec<Vec<bool>>) -> Region {
    let c = map.map[pos.0 as usize][pos.1 as usize];
    let mut region = Region { char: c, area: 0, perimeter: 0 };
    expand_region(pos, map, visited, &mut region);
    return region;
}
const DIR: [(isize, isize);4] = [(1,0),(0,1),(-1,0),(0,-1)];
fn expand_region(pos: &Pos, map: &MapData, visited: &mut Vec<Vec<bool>>, region: &mut Region) {
    visited[pos.0 as usize][pos.1 as usize] = true;
    region.area += 1;
    for dir in DIR.iter() {
        let npos = (pos.0 + dir.0, pos.1 + dir.1);
        if is_pos_inside(&npos, map) && map.map[npos.0 as usize][npos.1 as usize] == region.char{
            if !visited[npos.0 as usize][npos.1 as usize] {
                expand_region(&npos, map, visited, region);
            }
        } else {
            region.perimeter += 1;
        }
    }
}

#[derive(Debug)]
struct Region2 {
    pub char: char,
    pub area: usize,
    pub fences: [Vec<Vec<usize>>;4],
}
fn get_region2(pos: &Pos, map: &MapData, visited: &mut Vec<Vec<bool>>) -> Region2 {
    let c = map.map[pos.0 as usize][pos.1 as usize];
    let inner_vec = vec![vec![]; map.col_count + 1];
    let fences = [inner_vec.clone(), inner_vec.clone(), inner_vec.clone(), inner_vec.clone()];
    let mut region = Region2 { char: c, area: 0, fences };
    expand_region2(pos, map, visited, &mut region);
    return region;
}
fn expand_region2(pos: &Pos, map: &MapData, visited: &mut Vec<Vec<bool>>, region: &mut Region2) {
    visited[pos.0 as usize][pos.1 as usize] = true;
    region.area += 1;
    for (i, dir) in DIR.iter().enumerate() {
        let npos = (pos.0 + dir.0, pos.1 + dir.1);
        if is_pos_inside(&npos, map) && map.map[npos.0 as usize][npos.1 as usize] == region.char{
            if !visited[npos.0 as usize][npos.1 as usize] {
                expand_region2(&npos, map, visited, region);
            }
        } else if dir.0 == 0 {
            // vertical side (|)
            region.fences[i][pos.1 as usize].push(pos.0 as usize);
        } else {
            // horizontal size (-)
            region.fences[i][pos.0 as usize].push(pos.1 as usize);
        }
    }
}
fn count_region2_side(mut region: Region2) -> Region {
    let count: usize = region.fences.iter_mut().map(|fences| {
        fences.iter_mut().map(|arr| count_sides(arr)).sum::<usize>()
    }).sum::<usize>();
    Region {
        char: region.char,
        area: region.area,
        perimeter: count
    }
}
fn count_sides(arr: &mut [usize]) -> usize {
    if arr.len() < 2 {
        return arr.len();
    }
    arr.sort();
    1 + arr.windows(2).filter(|w| w[0]+1 != w[1]).count()
}

fn parse_input(input: &str) -> MapData {
    let map = input.lines()
        .map(|l| l.chars().collect())
        .collect::<Vec<Vec<char>>>();
    let row_count = map.len();
    let col_count = map[0].len();
    MapData { map, row_count, col_count}
}