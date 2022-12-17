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
    return line


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day17/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day17/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


shapes = {
    'minus': [(0, 0), (0, 1), (0, 2), (0, 3)],
    'plus': [(0, 1), (-1, 0), (-1, 1), (-1, 2), (-2, 1)],
    'l': [(0, 0), (0, 1), (0, 2), (-1, 2), (-2, 2)],
    'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
    'square': [(0, 0), (0, 1), (-1, 0), (-1, 1)],
}


def can_move(board, shape, leftmost, botmost):
    for r, c in shape:
        if botmost + r >= len(board):
            return False 
        if botmost + r < 0:
            continue
        if not(0 <= leftmost + c < 7):
            return False
        if board[botmost + r][leftmost + c] == '#':
            return False
    return True


def update(board, shape, leftmost, botmost):
    for r, c in shape:
        board[botmost + r][leftmost + c] = '#'


def highest(board):
    for r, row in enumerate(board):
        if '#' in row:
            return r
    return len(board)


def part1(inp):
    global shapes
    board = [['.' for _ in range(7)] for _ in range(4)]
    names = ['minus', 'plus', 'l', 'I', 'square']
    nextmove = 0

    for t in range(2022):
        name = names[t % 5]
        leftmost = 2
        botmost = 0
        shape = shapes[name]
        while True:
            potential_leftmost = leftmost + (1 if inp[nextmove] == '>' else -1)
            if can_move(board, shape, potential_leftmost, botmost):
                leftmost = potential_leftmost
            nextmove = (nextmove + 1) % len(inp)
            if can_move(board, shape, leftmost, botmost + 1):
                botmost += 1
            else:
                break

        update(board, shape, leftmost, botmost)
        h = highest(board)
        if h < 4:
            board = [['.' for _ in range(7)] for _ in range(4 - h)] + board

    return len(board) - highest(board)


def top_rows(board):
    rows = ''
    # Just randomly take top 6 filled rows as a signature
    for row in board[4:10]:
        rows += ''.join(row)
    return rows


def part2(inp):
    global shapes
    board = [['.' for _ in range(7)] for _ in range(4)]
    names = ['minus', 'plus', 'l', 'I', 'square']
    M = 1000000000000
    nextmove = 0
    seen = {} # (pattern, nextmove, name) -> time
    heights = {}

    t = 0
    while True:
        name = names[t % 5]
        leftmost = 2
        botmost = 0
        shape = shapes[name]
        heights[t] = len(board) - highest(board)
        rows = top_rows(board)
        if (rows, nextmove, name) in seen:
            tseen = seen[(rows, nextmove, name)]
            repeat = t - tseen
            hrepeat = heights[t] - heights[tseen]
            # This works and if I think hard enough about it also seems to makes sense
            # Essentially: for each full repeat add its height and then add the remainder
            return ((M - tseen) // repeat) * hrepeat + heights[tseen + ((M - tseen) % repeat)]
        seen[(rows, nextmove, name)] = t

        while True:
            potential_leftmost = leftmost + (1 if inp[nextmove] == '>' else -1)
            if can_move(board, shape, potential_leftmost, botmost):
                leftmost = potential_leftmost
            nextmove = (nextmove + 1) % len(inp)
            if can_move(board, shape, leftmost, botmost + 1):
                botmost += 1
            else:
                break

        update(board, shape, leftmost, botmost)
        h = highest(board)
        if h < 4:
            board = [['.' for _ in range(7)] for _ in range(4 - h)] + board

        t += 1


if __name__ == '__main__':
    main()

