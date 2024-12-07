use std::fs; 
use std::time::Instant;
use colored::*;
mod d1; mod d2; mod d3; mod d4; mod d5; mod d5b;
mod d6; mod d7;

fn main() {
    print_header();
    
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
}

fn print_day(num: usize) {
    println!("{}", format!("Day {num}").bold());
}

type Solver = &'static dyn Fn(&String) -> String;
fn run(solver: Solver, name: &str, input_filename: &str) {
    let input_str = read_input(input_filename);
    let start = Instant::now();
    let result = solver(&input_str);
    let duration = start.elapsed();
    println!("  {}: {} {}", name.bold(), result, formatted_duration(duration).yellow());
}

fn read_input(filename: &str) -> String {
    let full_path = format!("inputs/{}", filename);
    return fs::read_to_string(full_path).expect("Could not read file");
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