use itertools::Itertools;
fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    include_str!("../../inputs/day02/input")
        .lines()
        .map(|line| line
            .split('x')
            .map(|v| v.to_string().parse::<i32>().unwrap())
            .collect::<Vec<i32>>())
        .fold(0, |acc, vals| {
            let (s1, s2, s3) = (vals[0] * vals[1], vals[0] * vals[2], vals[1] * vals[2]);
            let box_wrapping = 2 * s1 + 2 * s2 + 2 * s3 + vec![s1, s2, s3].iter().min().unwrap();
            acc + box_wrapping
        })
}

fn part2() -> i32 {
    include_str!("../../inputs/day02/input")
        .lines()
        .map(|line| line
            .split('x')
            .map(|v| v.to_string().parse::<i32>().unwrap())
            .sorted()
            .collect::<Vec<i32>>())
        .fold(0, |acc, vals| {
            let wrap_around = vals.iter().fold(1, |acc, v| acc * v);
            let bow = 2 * vals.iter().sum::<i32>() - 2 * vals.iter().max().unwrap();
            acc + wrap_around + bow
        })
}