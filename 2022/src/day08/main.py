import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return list(map(int, list(line)))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day08/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day08/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    count = 0
    for r in range(len(inp)):
        for c in range(len(inp[r])):
            tree_val = inp[r][c]
            max_height = -1
            for _r in range(r):
                max_height = max(max_height, inp[_r][c])
            if max_height < tree_val:
                count += 1
                continue
            max_height = -1
            for _r in range(r + 1, len(inp)):
                max_height = max(max_height, inp[_r][c])
            if max_height < tree_val:
                count += 1
                continue
            max_height = -1
            for _c in range(c):
                max_height = max(max_height, inp[r][_c])
            if max_height < tree_val:
                count += 1
                continue
            max_height = -1
            for _c in range(c + 1, len(inp[r])):
                max_height = max(max_height, inp[r][_c])
            if max_height < tree_val:
                count += 1
                continue
    return count


def part2(inp):
    score = 0
    for r in range(len(inp)):
        for c in range(len(inp[r])):
            prod = 1
            seen = 0
            for _r in range(r - 1, -1, -1):
                seen += 1
                if inp[_r][c] >= inp[r][c]:
                    break
            prod *= seen
            seen = 0
            for _r in range(r + 1, len(inp)):
                seen += 1
                if inp[_r][c] >= inp[r][c]:
                    break
            prod *= seen
            seen = 0
            for _c in range(c - 1, -1, -1):
                seen += 1
                if inp[r][_c] >= inp[r][c]:
                    break
            prod *= seen
            seen = 0
            for _c in range(c + 1, len(inp[r])):
                seen += 1
                if inp[r][_c] >= inp[r][c]:
                    break
            prod *= seen
            score = max(score, prod)
    return score


if __name__ == '__main__':
    main()

