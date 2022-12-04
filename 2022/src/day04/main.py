import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    a, b = line.split(',')
    return list(map(int, a.split('-'))), list(map(int, b.split('-')))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day04/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day04/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    res = 0
    for (a, b), (c, d) in inp:
        if a <= c and b >= d:
            res += 1
        elif c <= a and d >= b:
            res += 1
    return res


def part2(inp):
    res = len(inp)
    for (a, b), (c, d) in inp:
        if b < c:
            res -= 1
        elif d < a:
            res -= 1
    return res


if __name__ == '__main__':
    main()

