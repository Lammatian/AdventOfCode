use std::collections::HashSet;

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    let directions = include_str!("../../inputs/day03/input").chars();

    let (mut x, mut y) = (0, 0);
    let mut visited = HashSet::new();
    visited.insert((x, y));

    for direction in directions {
        match direction {
            '^' => y += 1,
            '>' => x += 1,
            'v' => y -= 1,
            '<' => x -= 1,
            _ => panic!("Unknown direction")
        }
        visited.insert((x, y));
    }

    visited.len() as i32
}

fn part2() -> i32 {
    let directions = include_str!("../../inputs/day03/input").chars();

    let mut xs = [0, 0];
    let mut ys = [0, 0];
    let mut visited = HashSet::new();
    visited.insert((0, 0));

    for (i, direction) in directions.enumerate() {
        match direction {
            '^' => ys[i % 2] += 1,
            '>' => xs[i % 2] += 1,
            'v' => ys[i % 2] -= 1,
            '<' => xs[i % 2] -= 1,
            _ => panic!("Unknown direction")
        }
        visited.insert((xs[i % 2], ys[i % 2]));
    }

    visited.len() as i32
}