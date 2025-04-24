#![allow(clippy::needless_return)]

use std::{env, fs}; 
use std::io::{stdout, Write};
use std::time::{Duration, Instant};
use colored::*;
mod utils;
mod intcode;
mod d01; 
mod d02; 

fn main() {
    let arg = env::args().nth(1);
    
    let n: usize = arg.as_deref()
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);
    
    print_header();
    let mut p = Printer::new(40, n);
    
    if true {
        // p.print_day(1);
        // p.run(&d01::part1, "Part 1", "01.txt");
        // p.run(&d01::part2, "Part 2", "01.txt");
        
        p.print_day(2);
        p.run(&d02::part1, "Part 1", "02.txt");
        p.run(&d02::part2, "Part 2", "02.txt");
        
        p.done();
        println!();
        println!("{}: {}", "Total".bold(), formatted_duration(p.total_duration).yellow());
    }
    
}

type Solver = &'static dyn Fn(&str) -> String;
struct Printer {
    row: usize,
    col: usize,
    max_row: usize,
    column_width: usize,
    run_count: usize,
    total_duration: Duration
}
impl Printer {
    fn new(column_width: usize, run_count: usize) -> Self {
        Self { col: 1, row: 1, max_row: 1, column_width, run_count, total_duration: Duration::new(0, 0) }
    }
    // fn println(&mut self, s: String) {
    //     println!("{}", s);
    // }
    fn println(&mut self, s: String) {
        print!("{}", s);
        if self.row >= self.max_row {
            println!();
        } else {
            print!("\x1b[1B");
        }
        print!("\x1b[{}G", self.col);
        stdout().flush().unwrap();
        self.row += 1;
        self.max_row = self.max_row.max(self.row);
    }
    fn next_col(&mut self) {
        print!("\x1b[{}A", self.row - 1);
        self.row = 1;
        self.col += self.column_width;
        print!("\x1b[{}G", self.col);
        stdout().flush().unwrap();
    }
    fn done(&mut self){
        print!("\x1b[1G");
        print!("\x1b[{}B", self.max_row - self.row);
    }
    
    fn print_day(&mut self, num: usize) {
        self.println("".to_string());
        let s = format!("{}", format!("Day {num}").bold());
        self.println(s);
    }
    fn run(&mut self, solver: Solver, name: &str, input_filename: &str) {
        let input_str = read_input(input_filename);
        let start = Instant::now();
        let result = solver(&input_str);
        for _ in 0..self.run_count-1 {
            solver(&input_str);
        }
        let duration = start.elapsed()/self.run_count as u32;
        self.total_duration += duration;
        let s = format!("  {}: {} {}", name.bold().dimmed(), result, formatted_duration(duration));
        self.println(s);
    }
}


fn print_header() {
    println!("Advent of Code 2024 in Rust");
}
fn read_input(filename: &str) -> String {
    let full_path = format!("inputs/{}", filename);
    fs::read_to_string(full_path).expect("Could not read file")
}
fn formatted_duration(duration: std::time::Duration) -> ColoredString {
    if duration.as_secs() > 0 {
        format!("({:.2} s)", duration.as_secs_f64()).into()
    } else if duration.as_millis() > 0 {
        let millis = duration.as_secs_f64() * 1000.0;
        format!("({:.2} ms)", millis).yellow().bold()
    } else if duration.as_micros() > 0 {
        let micros = duration.as_micros();
        format!("({:.2} us)", micros).yellow()
    } else {
        format!("({} ns)", duration.as_nanos()).dimmed()
    }
}