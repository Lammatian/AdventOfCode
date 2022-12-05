import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    if line[0] != "m":
        line = line.split('\n')
        cranes = []
        for l in line:
            cranes.append([l[i+1:i+2] for i in range(0, len(l), 4)])
        return cranes[:-1]
    else:
        line = line.split('\n')[:-1]
        moves = []
        for l in line:
            moves.append([int(l.split()[1]), int(l.split()[3]), int(l.split()[-1])])
        return moves


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day05/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day05/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().split('\n\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    cranes = inp[0]
    moves = inp[1]
    cs = [[] for _ in range(9)]
    for c in cranes[::-1]:
        for i in range(len(c)):
            if c[i] != " ":
                cs[i].append(c[i])

    for c, f, t in moves:
        for i in range(c):
            cs[t-1].append(cs[f-1].pop())

    return ''.join(c[-1] for c in cs)


def part2(inp):
    cranes = inp[0]
    moves = inp[1]
    cs = [[] for _ in range(9)]
    for c in cranes[::-1]:
        for i in range(len(c)):
            if c[i] != " ":
                cs[i].append(c[i])

    for c, f, t in moves:
        cs[t-1] += cs[f-1][-c:]
        cs[f-1] = cs[f-1][:-c]

    return ''.join(c[-1] for c in cs)


if __name__ == '__main__':
    main()

