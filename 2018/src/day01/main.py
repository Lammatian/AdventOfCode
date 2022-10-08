import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day01/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day01/input'

    with open(filepath) as f:
        inp = list(map(lambda x: int(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    return sum(inp)


def part2(inp):
    freqs = set([0])
    curr = 0
    while True:
        for v in inp:
            curr += v 
            if curr in freqs:
                return curr
            freqs.add(curr)


if __name__ == '__main__':
    main()

