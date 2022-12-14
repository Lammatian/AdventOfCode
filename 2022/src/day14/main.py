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
    x, y = 100, 0
    while y < maxy:
        if board[y+1][x] == '.':
            y += 1
        elif board[y+1][x] in 'o#':
            if board[y+1][x-1] == '.':
                x -= 1
                y += 1
            elif board[y+1][x+1] == '.':
                x += 1
                y += 1
            else:
                return (x, y)
    return None


def part1(inp):
    maxy = 0
    for row in inp:
        for x, y in row:
            maxy = max(maxy, y)
    board = [['.' for _ in range(200)] for _ in range(maxy+1)]
    for row in inp:
        for (xs, ys), (xe, ye) in zip(row, row[1:]):
            if xs == xe:
                for y in range(min(ys,ye), max(ye,ys)+1):
                    board[y][xs - 400] = '#'
            else:
                for x in range(min(xs,xe), max(xs,xe)+1):
                    board[ys][x - 400] = '#'

    count = 0
    while True:
        d = dropped(board, maxy)
        if d is None:
            break
        board[d[1]][d[0]] = 'o'
        count += 1
    return count



def part2(inp):
    maxy = 0
    for row in inp:
        for x, y in row:
            maxy = max(maxy, y)
    maxy += 2
    board = [['.' for _ in range(800)] for _ in range(maxy+1)]
    board[maxy] = list('#' * 800)
    for row in inp:
        for (xs, ys), (xe, ye) in zip(row, row[1:]):
            if xs == xe:
                for y in range(min(ys,ye), max(ye,ys)+1):
                    board[y][xs - 400] = '#'
            else:
                for x in range(min(xs,xe), max(xs,xe)+1):
                    board[ys][x - 400] = '#'

    count = 0
    while True:
        d = dropped(board, maxy)
        if d is None:
            print('Error')
            return
        if d == (100, 0):
            count += 1
            break
        board[d[1]][d[0]] = 'o'
        count += 1
    return count


if __name__ == '__main__':
    main()

