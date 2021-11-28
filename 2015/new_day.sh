#!/bin/bash
DAY=$1

printf -v DAYNUM "%02d" $DAY
mkdir "inputs/day${DAYNUM}"
mkdir "src/day${DAYNUM}"

cat <<EOT >> src/day${DAYNUM}/main.rs
fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    1
}

fn part2() -> i32 {
    2
}
EOT

cat <<EOT >> Cargo.toml

[[bin]]
name = "day${DAYNUM}"
path = "src/day${DAYNUM}/main.rs"
EOT