#![allow(clippy::needless_return)]

use std::fs; 
use std::io::{stdout, Write};
use std::time::Instant;
use colored::*;
mod utils;
mod d01; mod d02; mod d03; mod d04; mod d05; mod d05b;
mod d06; mod d07; mod d08; mod d09; mod d10;
mod d11; mod d11b; mod d12; mod d13; mod d14; mod d15;
mod d16; mod d17; mod d18; mod d19; mod d20;

fn main() {
    print_header();
    let mut p = Printer::new(40);
    
    if true {
        p.print_day(1);
        p.run(&d01::part1, "Part 1", "01.txt");
        p.run(&d01::part2, "Part 2", "01.txt");
        
        p.print_day(2);
        p.run(&d02::part1, "Part 1", "02.txt");
        p.run(&d02::part2, "Part 2", "02.txt");
        
        p.print_day(3);
        p.run(&d03::part1, "Part 1", "03.txt");
        p.run(&d03::part2, "Part 2", "03.txt");
        
        p.print_day(4);
        p.run(&d04::part1, "Part 1", "04.txt");
        p.run(&d04::part2, "Part 2", "04.txt");
        
        p.print_day(5);
        // printer.run(&d05::part1, "Part 1", "05.txt");
        // printer.run(&d05::part2, "Part 2", "05.txt");
        p.run(&d05b::part1, "Part 1", "05.txt");
        p.run(&d05b::part2, "Part 2", "05.txt");
        
        p.print_day(6);
        p.run(&d06::part1, "Part 1", "06.txt");
        p.run(&d06::part2, "Part 2", "06.txt");
        
        p.print_day(7);
        p.run(&d07::part1, "Part 1", "07.txt");
        p.run(&d07::part2, "Part 2", "07.txt");
        
        p.print_day(8);
        p.run(&d08::part1, "Part 1", "08.txt");
        // printer.run(&d08::part2, "Part 2", "08.txt");
        p.run(&d08::part2b, "Part 2", "08.txt");
        
        p.print_day(9);
        p.run(&d09::part1, "Part 1", "09.txt");
        p.run(&d09::part2, "Part 2", "09.txt");
        
        p.print_day(10);
        p.run(&d10::part1, "Part 1", "10.txt");
        p.run(&d10::part2, "Part 2", "10.txt");
        
        p.next_col();
        
        p.print_day(11);
        p.run(&d11b::part1, "Part 1", "11.txt");
        p.run(&d11b::part2, "Part 2", "11.txt");
        
        p.print_day(12);
        p.run(&d12::part1, "Part 1", "12.txt");
        p.run(&d12::part2, "Part 2", "12.txt");
        
        p.print_day(13);
        p.run(&d13::part1, "Part 1", "13.txt");
        p.run(&d13::part2, "Part 2", "13.txt");
        
        p.print_day(14);
        p.run(&d14::part1, "Part 1", "14.txt");
        p.run(&d14::part2, "Part 2", "14.txt");
        
        p.print_day(15);
        p.run(&d15::part1, "Part 1", "15.txt");
        p.run(&d15::part2, "Part 2", "15.txt");
        
        p.print_day(16);
        p.run(&d16::part1, "Part 1", "16.txt");
        p.run(&d16::part2, "Part 2", "16.txt");
        
        p.print_day(17);
        p.run(&d17::part1, "Part 1", "17.txt");
        p.run(&d17::part2, "Part 2", "17.txt");
        
        p.print_day(18);
        p.run(&d18::part1, "Part 1", "18.txt");
        p.run(&d18::part2, "Part 2", "18.txt");
        
        p.print_day(19);
        p.run(&d19::part1, "Part 1", "19.txt");
        p.run(&d19::part2, "Part 2", "19.txt");
        
        p.print_day(20);
        p.run(&d20::part1, "Part 1", "20.txt");
        p.run(&d20::part2, "Part 2", "20.txt");
        
        p.done();
    }
    
}

type Solver = &'static dyn Fn(&str) -> String;
struct Printer {
    row: usize,
    col: usize,
    max_row: usize,
    column_width: usize
}
impl Printer {
    fn new(column_width: usize) -> Self {
        Self { col: 1, row: 1, max_row: 1, column_width }
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
        let duration = start.elapsed();
        let s = format!("  {}: {} {}", name.bold().dimmed(), result, formatted_duration(duration).yellow());
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