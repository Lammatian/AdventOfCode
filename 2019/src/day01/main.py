import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return int(line)


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
    return sum((x // 3) - 2 for x in inp)


def part2(inp):
    reqs = 0
    while inp:
        req = inp.pop(0)
        if req // 3 - 2 <= 0:
            continue
        reqs += (req // 3) - 2
        inp.append(req // 3 - 2)
    return reqs


if __name__ == '__main__':
    main()

