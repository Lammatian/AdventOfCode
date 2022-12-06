import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day06/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day06/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    for i in range(len(inp)):
        if len(set(inp[i:i+4])) == 4:
            return i + 4


def part2(inp):
    for i in range(len(inp)):
        if len(set(inp[i:i+14])) == 14:
            return i + 14


if __name__ == '__main__':
    main()

