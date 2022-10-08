import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day02/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day02/input'

    with open(filepath) as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    twos = 0
    threes = 0
    for c in inp:
        ct = Counter(c)
        if 2 in ct.values():
            twos += 1
        if 3 in ct.values():
            threes += 1
    return twos * threes


def part2(inp):
    min_diff = 10**5
    A = None
    B = None
    for a in inp:
        for b in inp:
            diff = 0
            if a == b:
                continue
            for c, d in zip(a, b):
                if c != d:
                    diff += 1
            if diff < min_diff:
                A = a
                B = b
                min_diff = min(min_diff, diff)
    result = ""
    for c, d in zip(A, B):
        if c == d:
            result += c
    return result


if __name__ == '__main__':
    main()

