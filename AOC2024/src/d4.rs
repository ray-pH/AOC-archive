pub fn part1(input: &String) -> String {
    let b = parse_input(input);
    let mut count = 0;
    for row in 0..b.height {
        for col in 0..b.width {
            if b.board[row as usize][col as usize] == 'X' {
                count += count_xmas(&b, row, col);
            }
        }
    }
    return count.to_string();
}

pub fn part2(input: &String) -> String {
    let mut count = 0;
    let b = parse_input(input);
    for row in 1..b.height-1 {
        for col in 1..b.width-1 {
            if b.board[row as usize][col as usize] == 'A' && is_x_mas(&b, row, col) {
                count += 1;
            }
        }
    }
    return count.to_string();
}

struct Board {
    board: Vec<Vec<char>>,
    width: i32,
    height: i32,
}

const DIRS: [(i32, i32); 8] = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)];
fn count_xmas(b: &Board, row: i32, col: i32) -> i32 {
    DIRS.map(|(row_dir, col_dir)| is_xmas_dir(b, row, col, row_dir, col_dir))
        .iter().filter(|bool| **bool).count() as i32
}
fn is_xmas_dir(b: &Board, row: i32, col: i32, row_dir: i32, col_dir: i32) -> bool {
    let row_final = row + 3 * row_dir;
    let col_final = col + 3 * col_dir;
    let valid_pos = 0 <= row_final && row_final < b.height && 0 <= col_final && col_final < b.width;
    return valid_pos &&
        b.board[(row + 1*row_dir) as usize][(col + 1*col_dir) as usize] == 'M' &&
        b.board[(row + 2*row_dir) as usize][(col + 2*col_dir) as usize] == 'A' &&
        b.board[(row + 3*row_dir) as usize][(col + 3*col_dir) as usize] == 'S';
}

fn is_x_mas(b: &Board, row: i32, col: i32) -> bool {
    let top_left = &b.board[(row - 1) as usize][(col - 1) as usize];
    let top_right = &b.board[(row - 1) as usize][(col + 1) as usize];
    let bottom_left = &b.board[(row + 1) as usize][(col - 1) as usize];
    let bottom_right = &b.board[(row + 1) as usize][(col + 1) as usize];
    return is_ms_or_sm((top_left, bottom_right)) && is_ms_or_sm((top_right, bottom_left));
}
fn is_ms_or_sm((a,b): (&char, &char)) -> bool {
    (*a == 'M' && *b == 'S') || (*a == 'S' && *b == 'M')
}

fn parse_input(input: &String) -> Board {
    let board = input.lines()
        .map(|line| line.chars().collect())
        .collect::<Vec<Vec<char>>>();
    let width = board[0].len() as i32;
    let height = board.len() as i32;
    return Board {board, width, height};
}