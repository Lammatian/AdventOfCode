use itertools::Itertools;
use itertools::FoldWhile::{Continue, Done};

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    include_str!("../../inputs/day01/input")
        .chars()
        .fold(0, |acc, c| acc + if c == '(' {1} else {-1})
}

fn part2() -> i32 {
    include_str!("../../inputs/day01/input")
        .chars()
        .enumerate()
        .fold_while(0, |acc, (steps, c)| {
            let change: i32 = if c == '(' {1} else {-1};
            if acc < 0 { Done(steps as i32) } else { Continue(acc + change) }
        }).into_inner()
}