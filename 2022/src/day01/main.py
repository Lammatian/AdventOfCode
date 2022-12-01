import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return sum(map(int, line.split()))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day01/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day01/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    return max(inp)


def part2(inp):
    return sum(sorted(inp)[-3:])


if __name__ == '__main__':
    main()

