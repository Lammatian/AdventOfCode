#!/bin/bash
DAY=$1

printf -v DAYNUM "%02d" $DAY
mkdir -p "inputs/day${DAYNUM}"
mkdir -p "src/day${DAYNUM}"

cat <<EOT > src/day${DAYNUM}/main.rs
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

grep -qxF "name = \"day${DAYNUM}\"" Cargo.toml || cat <<EOT >> Cargo.toml

[[bin]]
name = "day${DAYNUM}"
path = "src/day${DAYNUM}/main.rs"
EOT

if [ ! -f "inputs/day${DAYNUM}/input" ]; then
    curl "https://adventofcode.com/2015/day/${DAY}/input" --cookie "session=${AOC_SESSION}" > "inputs/day${DAYNUM}/input"
fi