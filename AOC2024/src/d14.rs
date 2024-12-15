use super::utils::positive_mod;
// use std::io::{self, Read};
// use std::process::Command;
// use image::{ImageBuffer, Luma};

pub fn part1(input: &str) -> String {
    let t = 1000;
    // let room_size = (11, 7);
    let room_size = (101, 103);
    let half_room_size = (room_size.0/2, room_size.1/2);
    let robots = parse_input(input);
    let final_pos = robots.iter().map(|r| calc_final_pos(r, t, room_size));
    let mut q = [0; 5];
    final_pos.for_each(|(x,y)| q[get_quadrant((x, y), half_room_size)] += 1);
    return (q[1] * q[2] * q[3] * q[4]).to_string();
}
pub fn part2(input: &str) -> String {
    let room_size = (101, 103);
    let mut robots = parse_input(input);
    for i in 0..10000 {
        step(&mut robots, room_size, 1);
        let img = display(&robots, room_size);
        // insight from exploring the images
        let mut c36 = 0;
        let mut c68 = 0;
        for (_, row) in img.iter().enumerate() {
            for (x, pixel) in row.iter().enumerate() {
                if x == 36 && *pixel == 255 { c36 += 1; }
                if x == 68 && *pixel == 255 { c68 += 1; }
            }
        }
        if c36 >= 31 && c68 >= 31 {
            return i.to_string();
        }
    }
    return "No solution found".to_string();
    // for i in 1..10000 {
    //     step(&mut robots, room_size, 1);
    //     let img = display(&robots, room_size);
    //     let mut imgbuf = ImageBuffer::new(room_size.1 as u32, room_size.0 as u32);
    //     let mut c36 = 0;
    //     let mut c68 = 0;
    //     for (y, row) in img.iter().enumerate() {
    //         for (x, pixel) in row.iter().enumerate() {
    //             imgbuf.put_pixel(x as u32, y as u32, Luma([*pixel as u8]));
    //             if x == 36 && *pixel == 255 { c36 += 1; }
    //             if x == 68 && *pixel == 255 { c68 += 1; }
    //         }
    //     }
    //     dbg!(c36);
    //     if c36 >= 31 && c68 >= 31 {
    //         let filename = format!("img/img{i}.png");
    //         imgbuf.save(filename).unwrap();
    //     }
    // }
    // todo!();
}

struct Robot {
    pub pos: (isize, isize),
    pub vel: (isize, isize)
}
fn calc_final_pos(robot: &Robot, t: isize, room_size: (isize, isize)) -> (isize, isize) {
    let x = robot.pos.0 + t * robot.vel.0;
    let y = robot.pos.1 + t * robot.vel.1;
    (positive_mod(x, room_size.0), positive_mod(y, room_size.1))
}

fn get_quadrant(pos: (isize, isize), halfsize: (isize, isize)) -> usize {
    let npos = (pos.0 - halfsize.0, pos.1 - halfsize.1);
    let x_positive = npos.0 > 0;
    let y_positive = npos.1 > 0;
    match (x_positive, y_positive) {
        _ if (npos.0 == 0 || npos.1 == 0) => 0,
        (true, true) => 1,
        (true, false) => 2,
        (false, true) => 3,
        (false, false) => 4,
    }
}

fn step(robots: &mut [Robot], room_size: (usize, usize), t: isize) {
    for robot in robots {
        let x = positive_mod(robot.pos.0 + robot.vel.0 * t, room_size.0 as isize);
        let y = positive_mod(robot.pos.1 + robot.vel.1 * t, room_size.1 as isize);
        robot.pos = (x, y);
    }
}
// fn display(robots: &[Robot], room_size: (usize, usize)) -> String {
//     let mut chars = vec![vec!['.'; room_size.1]; room_size.0];
//     for robot in robots {
//         chars[robot.pos.0 as usize][robot.pos.1 as usize] = '#';
//     }
//     return chars.iter().map(|x| x.iter().collect::<String>()).collect::<Vec<String>>().join("\n");
// }
fn display(robots: &[Robot], room_size: (usize, usize)) -> Vec<Vec<usize>> {
    let mut img = vec![vec![0; room_size.1]; room_size.0];
    for robot in robots {
        img[robot.pos.0 as usize][robot.pos.1 as usize] = 255;
    }
    return img;
}

fn parse_input(input: &str) -> Vec<Robot> {
    input.lines().map(parse_robot).collect()
}
fn parse_robot(input: &str) -> Robot {
    let mut parts = input.split_whitespace();
    let pos = parse_tuple(parts.next().unwrap());
    let vel = parse_tuple(parts.next().unwrap());
    Robot { pos, vel }
}
// X=0,4
fn parse_tuple(input: &str) -> (isize, isize) {
    let tuple_str = input.split('=').last().unwrap();
    let mut parts = tuple_str.split(',');
    let x = parts.next().unwrap().parse::<isize>().unwrap();
    let y = parts.next().unwrap().parse::<isize>().unwrap();
    (x, y)
}