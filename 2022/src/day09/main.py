import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line.split()


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day09/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day09/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def adjacent(hx, hy, tx, ty):
    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            if (tx + dx, ty + dy) == (hx, hy):
                return True
    return False


def part1(inp):
    seen = set()
    hpos = (0, 0)
    tpos = (0, 0)
    DIR = {
        'R': (1, 0),
        'L': (-1, 0),
        'U': (0, -1),
        'D': (0, 1),
    }
    seen.add(tpos)
    for d, v in inp:
        for _ in range(int(v)):
            dx, dy = DIR[d]
            hx, hy = hpos
            hx += dx
            hy += dy
            hpos = (hx, hy)
            if not adjacent(*hpos, *tpos):
                hx, hy = hpos
                tx, ty = tpos
                if hx - tx >= 2:
                    tx = hx - 1
                    ty = hy
                if tx - hx >= 2:
                    tx = hx + 1
                    ty = hy
                if hy - ty >= 2:
                    ty = hy - 1
                    tx = hx
                if ty - hy >= 2:
                    ty = hy + 1
                    tx = hx
                tpos = (tx, ty)
            seen.add(tpos)
    return len(seen)


def printpos(pos):
    board = []
    for _ in range(50):
        board.append(['.'] * 50)

    for x, y in pos:
        board[y+25][x+25] = '#'

    for row in board:
        print(''.join(row))


def printsnek(snek):
    board = []
    SIZE = 40
    for _ in range(SIZE):
        board.append(['.'] * SIZE)

    for i, (x, y) in enumerate(snek):
        print(i, x, y)
        board[y+SIZE//2][x+SIZE//2] = str(i)

    for row in board:
        print(''.join(row))

def printall(snek, pos):
    board = []
    SIZE = 40
    for _ in range(SIZE):
        board.append(['.'] * SIZE)

    for x, y in pos:
        board[y+SIZE//2][x+SIZE//2] = '#'

    for i in range(len(snek) - 1, -1, -1):
        x, y = snek[i]
        board[y+SIZE//2][x+SIZE//2] = str(i)

    for row in board:
        print(''.join(row))


def part2(inp):
    seen = set()
    pos = [(0, 0) for _ in range(10)]
    DIR = {
        'R': (1, 0),
        'L': (-1, 0),
        'U': (0, -1),
        'D': (0, 1),
    }
    seen.add(pos[-1])
    for d, v in inp:
        for _ in range(int(v)):
            dx, dy = DIR[d]
            hx, hy = pos[0]
            hx += dx
            hy += dy
            pos[0] = (hx, hy)
            for i in range(9):
                hpos = pos[i]
                tpos = pos[i + 1]
                if not adjacent(*hpos, *tpos):
                    hx, hy = hpos
                    tx, ty = tpos
                    if hx - tx >= 2 and hy - ty >= 2:
                        tx = hx - 1
                        ty = hy - 1
                    if hx - tx >= 2 and hy - ty <= -2:
                        tx = hx - 1
                        ty = hy + 1
                    if hx - tx <= -2 and hy - ty >= 2:
                        tx = hx + 1
                        ty = hy - 1
                    if hx - tx <= -2 and hy - ty <= -2:
                        tx = hx + 1
                        ty = hy + 1
                    if hx - tx >= 2:
                        tx = hx - 1
                        ty = hy
                    if tx - hx >= 2:
                        tx = hx + 1
                        ty = hy
                    if hy - ty >= 2:
                        ty = hy - 1
                        tx = hx
                    if ty - hy >= 2:
                        ty = hy + 1
                        tx = hx
                    tpos = (tx, ty)
                pos[i] = hpos
                pos[i + 1] = tpos
            seen.add(pos[-1])
    return len(seen)


if __name__ == '__main__':
    main()

