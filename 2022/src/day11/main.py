import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    _, a, b, c, d, e = line.split('\n')
    a = list(map(int, a.split(': ')[-1].split(', ')))
    b = [b.split()[-2], b.split()[-1]]
    c = int(c.split()[-1])
    d = int(d.split()[-1])
    e = int(e.split()[-1])
    return [a, b, c, d, e]


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day11/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day11/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n\n')))

    print(inp)
    inp1 = [[a[:], b, c, d, e] for a, b, c, d, e in inp]
    inp2 = [[a[:], b, c, d, e] for a, b, c, d, e in inp]

    print(part1(inp1))
    print(part2(inp2))


def part1(inp):
    inspected = defaultdict(int)
    for _ in range(20):
        for i, (items, op, test, tr, fa) in enumerate(inp):
            newItems = []
            inspected[i] += len(items)
            for item in items:
                if op[0] == '+':
                    newItems.append((item + int(op[1])) // 3)
                else:
                    if op[1] == 'old':
                        newItems.append((item * item) // 3)
                    else:
                        newItems.append((item * int(op[1])) // 3)
            for item in newItems:
                if item % test == 0:
                    inp[tr][0].append(item)
                else:
                    inp[fa][0].append(item)
            inp[i][0] = []

    v = sorted(inspected.values())
    return v[-1] * v[-2]


def part2(inp):
    mod = reduce(lambda x, y: x * y, [i[2] for i in inp], 1)
    inspected = defaultdict(int)
    items = [(i, item) for i, (its, _, _, _, _) in enumerate(inp) for item in its ]
    for _ in range(10000):
        for i, (items, op, test, tr, fa) in enumerate(inp):
            newItems = []
            inspected[i] += len(items)
            for item in items:
                if op[0] == '+':
                    newItems.append(item + int(op[1]))
                else:
                    if op[1] == 'old':
                        newItems.append(item * item)
                    else:
                        newItems.append(item * int(op[1]))
            for item in newItems:
                if item % test == 0:
                    inp[tr][0].append(item % mod)
                else:
                    inp[fa][0].append(item % mod)
            inp[i][0] = []

    v = sorted(inspected.values())
    return v[-1] * v[-2]


if __name__ == '__main__':
    main()

