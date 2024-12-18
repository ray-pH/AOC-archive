#![allow(clippy::needless_return)]

use std::fs; 
use std::time::Instant;
use colored::*;
mod utils;
mod d1; mod d2; mod d3; mod d4; mod d5; mod d5b;
mod d6; mod d7; mod d8; mod d9; mod d10;
mod d11; mod d11b; mod d12; mod d13; mod d14; mod d15;
mod d16; mod d17; mod d18;

fn main() {
    print_header();
    if true {
        print_day(1);
        run(&d1::part1, "Part 1", "1.txt");
        run(&d1::part2, "Part 2", "1.txt");
        
        print_day(2);
        run(&d2::part1, "Part 1", "2.txt");
        run(&d2::part2, "Part 2", "2.txt");
        
        print_day(3);
        run(&d3::part1, "Part 1", "3.txt");
        run(&d3::part2, "Part 2", "3.txt");
        
        print_day(4);
        run(&d4::part1, "Part 1", "4.txt");
        run(&d4::part2, "Part 2", "4.txt");
        
        print_day(5);
        // run(&d5::part1, "Part 1", "5.txt");
        // run(&d5::part2, "Part 2", "5.txt");
        run(&d5b::part1, "Part 1", "5.txt");
        run(&d5b::part2, "Part 2", "5.txt");
        
        print_day(6);
        run(&d6::part1, "Part 1", "6.txt");
        run(&d6::part2, "Part 2", "6.txt");
        
        print_day(7);
        run(&d7::part1, "Part 1", "7.txt");
        run(&d7::part2, "Part 2", "7.txt");
        
        print_day(8);
        run(&d8::part1, "Part 1", "8.txt");
        // run(&d8::part2, "Part 2", "8.txt");
        run(&d8::part2b, "Part 2", "8.txt");
        
        print_day(9);
        run(&d9::part1, "Part 1", "9.txt");
        run(&d9::part2, "Part 2", "9.txt");
        
        print_day(10);
        run(&d10::part1, "Part 1", "10.txt");
        run(&d10::part2, "Part 2", "10.txt");
        
        print_day(11);
        run(&d11b::part1, "Part 1", "11.txt");
        run(&d11b::part2, "Part 2", "11.txt");
        
        print_day(12);
        run(&d12::part1, "Part 1", "12.txt");
        run(&d12::part2, "Part 2", "12.txt");
        
        print_day(13);
        run(&d13::part1, "Part 1", "13.txt");
        run(&d13::part2, "Part 2", "13.txt");
        
        print_day(14);
        run(&d14::part1, "Part 1", "14.txt");
        run(&d14::part2, "Part 2", "14.txt");
        
        print_day(15);
        run(&d15::part1, "Part 1", "15.txt");
        run(&d15::part2, "Part 2", "15.txt");
        
        print_day(16);
        run(&d16::part1, "Part 1", "16.txt");
        run(&d16::part2, "Part 2", "16.txt");
        
        print_day(17);
        run(&d17::part1, "Part 1", "17.txt");
        run(&d17::part2, "Part 2", "17.txt");
        
        print_day(18);
        run(&d18::part1, "Part 1", "18.txt");
        run(&d18::part2, "Part 2", "18.txt");
    }
    
}

fn print_day(num: usize) {
    println!("{}", format!("Day {num}").bold());
}

type Solver = &'static dyn Fn(&str) -> String;
fn run(solver: Solver, name: &str, input_filename: &str) {
    let input_str = read_input(input_filename);
    let start = Instant::now();
    let result = solver(&input_str);
    let duration = start.elapsed();
    println!("  {}: {} {}", name.bold(), result, formatted_duration(duration).yellow());
}

fn read_input(filename: &str) -> String {
    let full_path = format!("inputs/{}", filename);
    fs::read_to_string(full_path).expect("Could not read file")
}
fn formatted_duration(duration: std::time::Duration) -> String {
    if duration.as_secs() > 0 {
        format!("({} s)", duration.as_secs())
    } else if duration.as_millis() > 0 {
        format!("({} ms)", duration.as_millis())
    } else if duration.as_micros() > 0 {
        format!("({} us)", duration.as_micros())
    } else {
        format!("({} ns)", duration.as_nanos())
    }
}

fn print_header() {
    println!("Advent of Code 2024 in Rust");
}