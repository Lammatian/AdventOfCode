use md5;

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    let secret = include_str!("../../inputs/day04/input");
    let mut counter = 0;

    loop {
        let full_secret = format!("{}{}", secret, counter);
        let digest = md5::compute(full_secret.as_bytes());
        if digest[0] as i32 + digest[1] as i32 + (digest[2] >> 4) as i32 == 0 {
            break counter
        }
        counter += 1;
    }
}

fn part2() -> i32 {
    let secret = include_str!("../../inputs/day04/input");
    let mut counter = 0;

    loop {
        let full_secret = format!("{}{}", secret, counter);
        let digest = md5::compute(full_secret.as_bytes());
        if digest[0] as i32 + digest[1] as i32 + digest[2] as i32 == 0 {
            break counter
        }
        counter += 1;
    }
}