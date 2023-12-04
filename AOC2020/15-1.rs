use std::fs::File;
use std::io::prelude::*;

fn main(){
    let mut file = File::open("1-input.txt").expect("Err");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Err");
    let vec : Vec<i32> = contents.lines()
                            .map(|s| s.parse().unwrap())
                            .collect();
    for v in vec{
        println!("{}",v+2)
    }
    // println!("{:?}",vec);
}
