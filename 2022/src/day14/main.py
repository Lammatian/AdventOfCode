import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *
from copy import deepcopy


def parse(line):
    positions = line.split(' -> ')
    positions = list(map(lambda x: list(map(int, x.split(','))), positions))
    return positions


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day14/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day14/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def dropped(board, maxy):
    x, y = 500, 0
    while y < maxy:
        if (x, y+1) not in board:
            y += 1
        else:
            if (x-1,y+1) not in board:
                x -= 1
                y += 1
            elif (x+1,y+1) not in board:
                x += 1
                y += 1
            else:
                return (x, y)
    return (x, y)


def part1(inp):
    maxy = 0
    for row in inp:
        for x, y in row:
            maxy = max(maxy, y)
    board = set()
    for row in inp:
        for (xs, ys), (xe, ye) in zip(row, row[1:]):
            if xs == xe:
                for y in range(min(ys,ye), max(ye,ys)+1):
                    board.add((xs, y))
            else:
                for x in range(min(xs,xe), max(xs,xe)+1):
                    board.add((x, ys))

    count = 0
    while True:
        x, y = dropped(board, maxy)
        if y == maxy:
            break
        board.add((x, y))
        count += 1
    return count



def part2(inp):
    maxy = 0
    for row in inp:
        for x, y in row:
            maxy = max(maxy, y)
    maxy += 1
    board = set()
    for row in inp:
        for (xs, ys), (xe, ye) in zip(row, row[1:]):
            if xs == xe:
                for y in range(min(ys,ye), max(ye,ys)+1):
                    board.add((xs, y))
            else:
                for x in range(min(xs,xe), max(xs,xe)+1):
                    board.add((x, ys))

    count = 0
    while True:
        d = dropped(board, maxy)
        if d == (500, 0):
            count += 1
            break
        board.add(d)
        count += 1
    return count


if __name__ == '__main__':
    main()

