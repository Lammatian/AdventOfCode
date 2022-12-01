import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return int(line) if line else 0


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day01/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day01/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    most = 0
    i = 0
    while i < len(inp):
        curr = 0
        while i < len(inp) and inp[i] != 0:
            curr += inp[i]
            i += 1
        most = max(most, curr)
        curr = 0
        i += 1
    return most


def part2(inp):
    cals = []
    i = 0
    while i < len(inp):
        curr = 0
        while i < len(inp) and inp[i] != 0:
            curr += inp[i]
            i += 1
        cals.append(curr)
        i += 1
    cals = sorted(cals)
    return cals[-1] + cals[-2] + cals[-3]


if __name__ == '__main__':
    main()

