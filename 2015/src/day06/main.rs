use regex::Regex;

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn part1() -> i32 {
    let re = Regex::new(r"(toggle|turn off|turn on) (\d+),(\d+) through (\d+),(\d+)\n").unwrap();
    let input = include_str!("../../inputs/day06/input");
    let mut grid: [[bool; 1000]; 1000] = [[false; 1000]; 1000];

    for cap in re.captures_iter(input) {
        let (action, xs, ys, xe, ye) = (
            &cap[1],
            cap[2].parse::<usize>().unwrap(),
            cap[3].parse::<usize>().unwrap(),
            cap[4].parse::<usize>().unwrap(),
            cap[5].parse::<usize>().unwrap()
        );

        for x in xs..(xe+1) {
            for y in ys..(ye+1) {
                grid[y][x] = match action {
                    "turn on" => true,
                    "turn off" => false,
                    "toggle" => !grid[y][x],
                    _ => panic!("Unknown action")
                }
            }
        }
    }

    grid
        .iter()
        .fold(0, |acc, row| acc + row.iter().filter(|l| **l).count() as i32)
}

fn part2() -> i32 {
    let re = Regex::new(r"(toggle|turn off|turn on) (\d+),(\d+) through (\d+),(\d+)\n").unwrap();
    let input = include_str!("../../inputs/day06/input");
    let mut grid: [[i32; 1000]; 1000] = [[0; 1000]; 1000];

    for cap in re.captures_iter(input) {
        let (action, xs, ys, xe, ye) = (
            &cap[1],
            cap[2].parse::<usize>().unwrap(),
            cap[3].parse::<usize>().unwrap(),
            cap[4].parse::<usize>().unwrap(),
            cap[5].parse::<usize>().unwrap()
        );

        for x in xs..(xe+1) {
            for y in ys..(ye+1) {
                grid[y][x] = match action {
                    "turn on" => grid[y][x] + 1,
                    "turn off" => if grid[y][x] > 0 {grid[y][x] - 1} else {0},
                    "toggle" => grid[y][x] + 2,
                    _ => panic!("Unknown action")
                }
            }
        }
    }

    grid
        .iter()
        .fold(0, |acc, row| acc + row.iter().sum::<i32>() as i32)
}
