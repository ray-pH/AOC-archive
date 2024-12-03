use regex::Regex;

pub fn part1(input: &String) -> String {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    let result = re.captures_iter(input)
        .map(|args| args[1].parse::<i32>().unwrap() * args[2].parse::<i32>().unwrap())
        .sum::<i32>();
    result.to_string()
}

pub fn part2(input: &String) -> String {
    let re = Regex::new(r"mul\(\d+,\d+\)|(do\(\))|(don't\(\))").unwrap();
    let mut result = 0;
    let mut is_active = true;
    for capt in re.find_iter(input) {
        match capt.as_str() {
            "do()" =>  is_active = true,
            "don't()" => is_active = false,
            _ => {
                let args = capt.as_str()[4..capt.as_str().len()-1].split(",").collect::<Vec<&str>>();
                let mul = args[0].parse::<i32>().unwrap() * args[1].parse::<i32>().unwrap();
                if is_active { result += mul; }
            }
        }
    }
    return result.to_string();
}
