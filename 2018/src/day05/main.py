import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day05/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day05/input'

    with open(filepath) as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    inp = list(inp)
    removed = True
    while removed:
        removed = False
        for i in range(len(inp) - 1, 0, -1):
            if i >= len(inp):
                continue
            if inp[i].lower() == inp[i - 1].lower() and inp[i] != inp[i - 1]:
                inp.pop(i)
                inp.pop(i - 1)
                removed = True
    return len(inp)


def part2(inp):
    to_remove = set(inp.lower())
    min_len = 10**5
    for c in to_remove:
        result = part1(inp.replace(c, '').replace(c.upper(), ''))
        min_len = min(min_len, result)
    return min_len


if __name__ == '__main__':
    main()

