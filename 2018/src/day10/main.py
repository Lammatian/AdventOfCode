import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    line = line[line.find('<') + 1:]
    px = int(line[:line.find(',')])
    line = line[line.find(',') + 1:]
    py = int(line[:line.find('>')])
    line = line[line.find('<') + 1:]
    vx = int(line[:line.find(',')])
    line = line[line.find(',') + 1:]
    vy = int(line[:line.find('>')])
    return [px, py, vx, vy]


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day10/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day10/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def msg(inp, bound):
    max_x = max([px for (px, py, vx, vy) in inp])
    max_y = max([py for (px, py, vx, vy) in inp])
    min_x = min([px for (px, py, vx, vy) in inp])
    min_y = min([py for (px, py, vx, vy) in inp])
    bx = max_x - min_x + 1
    by = max_y - min_y + 1
    board = []
    for _ in range(by):
        board.append(['.'] * bx)

    for (px, py, _, _) in inp:
        if px - min_x < bx and py - min_y < by:
            board[py - min_y][px - min_x] = '#'

    for row in board:
        print(''.join(row))


def part1(inp):
    # Initial speed-up
    inp = [[px + 10000 * vx, py + 10000 * vy, vx, vy] for (px, py, vx, vy) in inp]
    last_boardsize = 1e10
    last_board = inp[:]
    max_x = max([px for (px, py, vx, vy) in inp])
    max_y = max([py for (px, py, vx, vy) in inp])
    min_x = min([px for (px, py, vx, vy) in inp])
    min_y = min([py for (px, py, vx, vy) in inp])
    curr_boardsize = (max_x - min_x) + (max_y - min_y)
    while curr_boardsize < last_boardsize:
        last_boardsize = curr_boardsize
        last_board = inp[:]
        for i in range(len(inp)):
            px, py, vx, vy = inp[i]
            px += vx
            py += vy
            inp[i] = [px, py, vx, vy]
        max_x = max([px for (px, py, vx, vy) in inp])
        max_y = max([py for (px, py, vx, vy) in inp])
        min_x = min([px for (px, py, vx, vy) in inp])
        min_y = min([py for (px, py, vx, vy) in inp])
        curr_boardsize = (max_x - min_x) + (max_y - min_y)

    msg(last_board, None)


def part2(inp):
    # Initial speed-up
    inp = [[px + 10000 * vx, py + 10000 * vy, vx, vy] for (px, py, vx, vy) in inp]
    moves = 10000
    last_boardsize = 1e10
    max_x = max([px for (px, py, vx, vy) in inp])
    max_y = max([py for (px, py, vx, vy) in inp])
    min_x = min([px for (px, py, vx, vy) in inp])
    min_y = min([py for (px, py, vx, vy) in inp])
    curr_boardsize = (max_x - min_x) + (max_y - min_y)
    while curr_boardsize < last_boardsize:
        last_boardsize = curr_boardsize
        for i in range(len(inp)):
            px, py, vx, vy = inp[i]
            px += vx
            py += vy
            inp[i] = [px, py, vx, vy]
        max_x = max([px for (px, py, vx, vy) in inp])
        max_y = max([py for (px, py, vx, vy) in inp])
        min_x = min([px for (px, py, vx, vy) in inp])
        min_y = min([py for (px, py, vx, vy) in inp])
        curr_boardsize = (max_x - min_x) + (max_y - min_y)
        moves += 1

    return moves - 1


if __name__ == '__main__':
    main()

