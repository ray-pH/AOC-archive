fn main(){
    let input = std::fs::read_to_string("1-input.txt").unwrap();
    let result = input
                .split('\n')
                .filter(|s| !s.is_empty())
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
    println!("{:?}",result);
    let n = result.len();
    for i in 0 .. n-1 {
        for j in i+1 .. n {
            if result[i] + result[j] == 2020 {
                println!("{}", result[i]*result[j]);
                return;
            }
        }
    }
}
