import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day06/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day06/input'

    with open(filepath) as f:
        inp = list(map(lambda x: list(map(int, x.split(','))), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def dist(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def part1(inp):
    size = 500
    board = [None] * size
    for i in range(size):
        board[i] = [None] * size

    for r in range(size):
        for c in range(size):
            min_dist = 10000
            min_pt = None
            contested = False
            for i, p in enumerate(inp):
                if dist((c, r), p) < min_dist:
                    min_pt = i
                    min_dist = dist((c, r), p)
                    contested = False
                elif dist((c, r), p) == min_dist:
                    contested = True 
            if contested:
                board[r][c] = '.'
            else:
                board[r][c] = min_pt

    infinite = set()
    for r, c in zip([0] * size, range(size)):
        infinite.add(board[r][c])
    for r, c in zip([size - 1] * size, range(size)):
        infinite.add(board[r][c])
    for c, r in zip([0] * size, range(size)):
        infinite.add(board[r][c])
    for c, r in zip([size - 1] * size, range(size)):
        infinite.add(board[r][c])
    print(infinite)

    sizes = defaultdict(int)
    for r in range(size):
        for c in range(size):
            sizes[board[r][c]] += 1

    return max(v for k, v in sizes.items() if k not in infinite)


def part2(inp):
    # Seemingly this returns the right answer so it's enough lol
    size = 2000
    area = 0
    for r in range(size):
        for c in range(size):
            if sum(dist((c, r), p) for p in inp) < 10000:
                area += 1
    return area


if __name__ == '__main__':
    main()

