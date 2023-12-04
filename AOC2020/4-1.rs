fn main(){
    let input = std::fs::read_to_string("4-ex.txt").unwrap();
    let passports = input
            .split("\n\n")
            .collect::<Vec<_>>();
    for p in passports{
        println!("{}\n",p);
    }
    // println!("{:?}",passports);
}
