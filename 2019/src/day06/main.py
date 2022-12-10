import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line.split(')')


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day06/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day06/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    o = {}
    seen = set()
    for f, t in inp:
        o[t] = f
        seen.add(f)
        seen.add(t)

    c = 0
    for p in seen:
        while p in o:
            p = o[p]
            c += 1
    return c


def part2(inp):
    o = {}
    seen = set()
    for f, t in inp:
        o[t] = f
        seen.add(f)
        seen.add(t)

    p = 'YOU'
    visited = [p]
    while p in o:
        p = o[p]
        visited.append(p)
    p2 = 'SAN'
    c = 0
    while p2 not in visited:
        p2 = o[p2]
        c += 1
    return c + visited.index(p2) - 2


if __name__ == '__main__':
    main()

