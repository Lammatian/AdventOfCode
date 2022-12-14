import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *
from copy import deepcopy


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day14/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day14/input'

    board = set()
    with open(filepath) as f:
        for line in f.read().strip().split('\n'):
            px = None
            py = None
            for xy in line.split(' -> '):
                x, y = xy.split(',')
                x = int(x)
                y = int(y)
                if px and py:
                    if x == px:
                        for y_ in range(min(y, py), max(y, py)+1):
                            board.add((x, y_))
                    else:
                        for x_ in range(min(x, px), max(x, px)+1):
                            board.add((x_, y))
                px, py = x, y
    print(board)
    print(part1(deepcopy(board)))
    print(part2(deepcopy(board)))


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


def part1(board):
    maxy = max(y for _, y in board)
    count = 0
    while True:
        x, y = dropped(board, maxy)
        if y == maxy:
            break
        board.add((x, y))
        count += 1
    return count


def part2(board):
    maxy = max(y for _, y in board) + 1

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

