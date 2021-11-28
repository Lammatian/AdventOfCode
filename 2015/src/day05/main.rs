fn main() {
    println!("{}", part1());
    println!("{}", part2());
}

fn is_nice1(word: &str) -> bool {
    word
        .chars()
        .filter(|c| "aeiou"
            .chars()
            .find(|c_| c_ == c)
            .is_some())
        .count() >= 3
        &&
    word
        .chars()
        .zip(word
            .chars()
            .skip(1))
        .any(|(c1, c2)| c1 == c2)
        &&
    !word
        .chars()
        .zip(word
            .chars()
            .skip(1))
        .any(|(c1, c2)| {
            [['a', 'b'], ['c', 'd'], ['p', 'q'], ['x', 'y']].contains(&[c1, c2])
        })
}

fn part1() -> i32 {
    include_str!("../../inputs/day05/input")
        .lines()
        .filter(|line| is_nice1(line))
        .count() as i32
}

fn is_nice2(word: &str) -> bool {
    let mut double_pair = false;
    let pairs = word.chars().zip(word.chars().skip(1)).collect::<Vec<(char, char)>>();
    let mut i = 0;
    'outer: loop {
        if i + 2 >= pairs.len() {
            break 'outer;
        }

        let mut j = i + 2;
        'inner: loop {
            if j >= pairs.len() {
                break 'inner;
            }

            if pairs[i] == pairs[j] {
                double_pair = true;
                break 'outer;
            }
            j += 1;
        }
        i += 1;
    }

    let oreo = word
        .chars()
        .zip(word
            .chars()
            .skip(2))
        .any(|(c1, c2)| c1 == c2);

    double_pair && oreo
}

fn part2() -> i32 {
    include_str!("../../inputs/day05/input")
        .lines()
        .filter(|line| is_nice2(line))
        .count() as i32
}
