use std::fs; 
use std::time::Instant;
use colored::*;
mod d1;

type Solver = &'static dyn Fn(&String) -> String;

fn main() {
    print_header();
    print_day(1);
    run(&d1::part1, "Part 1", "1.txt");
    run(&d1::part2, "Part 2", "1.txt");
}

fn print_day(num: usize) {
    println!("{}", format!("Day {num}").bold());
}
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