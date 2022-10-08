#!/bin/bash
if [ $# -eq 0 ]; then
    echo "Usage: ./new_day.sh DAYNUM"
    exit 1
fi

# Assumes this is put in directory named after the year number
YEAR=$(basename $(pwd))
DAY=$1

printf -v DAYNUM "%02d" $DAY
INP_PATH="inputs/day${DAYNUM}/input"
SRC_PATH="src/day${DAYNUM}/main.py"
mkdir -p $(dirname $INP_PATH)
mkdir -p $(dirname $SRC_PATH)

if [ ! -f $SRC_PATH ]; then
cat <<EOT > $SRC_PATH
import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../$(dirname $INP_PATH)/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../${INP_PATH}'

    with open(filepath) as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    pass


def part2(inp):
    pass


if __name__ == '__main__':
    main()

EOT
fi

if [ ! -f $INP_PATH ]; then
    curl "https://adventofcode.com/${YEAR}/day/${DAY}/input" --cookie "session=${AOC_SESSION}" > $INP_PATH
fi

# Start at line 17 i.e. where the input is read/parsed
vim -c ":17" $SRC_PATH
