#!/bin/bash
DAY=$1

printf -v DAYNUM "%02d" $DAY
mkdir -p "inputs/day${DAYNUM}"
mkdir -p "src/day${DAYNUM}"

if [ ! -f "src/day${DAYNUM}/main.py" ]; then
cat <<EOT > src/day${DAYNUM}/main.py
import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    pass


def part2(inp):
    pass


def main():
    with open(f'{dir_path}/../../inputs/day${DAYNUM}/input') as f:
        inp = f.read().strip()
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

EOT
fi

if [ ! -f "inputs/day${DAYNUM}/input" ]; then
    curl "https://adventofcode.com/2021/day/${DAY}/input" --cookie "session=${AOC_SESSION}" > "inputs/day${DAYNUM}/input"
fi