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
    print(part1_(deepcopy(inp)))
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
        if leftmost + c >= 7:
            return False
        if leftmost + c < 0:
            return False
        if board[botmost + r][leftmost + c] == '#':
            return False
    return True


def update_(board, shape, leftmost, botmost):
    for r, c in shape:
        board[botmost + r][leftmost + c] = '#'


def part1_(inp):
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

        update_(board, shape, leftmost, botmost)
        h = highest(board)
        if h < 4:
            board = [['.' for _ in range(7)] for _ in range(4 - h)] + board

    return len(board) - highest(board)


def touching(board, bottom, leftmost, botmost):
    for r, c in bottom:
        if botmost + r + 1 >= len(board):
            return True
        if botmost + r < 0:
            continue
        if board[botmost + r + 1][leftmost + c] == '#':
            return True
    return False


def right(board, right, leftmost, botmost):
    for r, c in right:
        if leftmost + c + 1 >= 7:
            return 0
        if botmost + r < 0:
            continue
        if board[botmost + r][leftmost + c + 1] == '#':
            return 0
    return 1


def left(board, left, leftmost, botmost):
    for r, c in left:
        if botmost + r < 0:
            continue
        if leftmost + c - 1 < 0:
            return 0
        if board[botmost + r][leftmost + c - 1] == '#':
            return 0
    return 1


def update(board, ss, leftmost, botmost):
    for r, c in ss:
        board[botmost + r][leftmost + c] = '#'


def highest(board):
    for r, row in enumerate(board):
        if '#' in row:
            return r
    return len(board)


def part1(inp):
    board = [['.' for _ in range(7)] for _ in range(4)]

    names = ['minus', 'plus', 'l', 'I', 'square']

    ss = { # row, col
        'minus': [(0, 0), (0, 1), (0, 2), (0, 3)],
        'plus': [(-1, 0), (0, 1), (-1, 1), (-1, 2), (-2, 1)],
        'l': [(0, 0), (0, 1), (0, 2), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 0), (0, 1), (-1, 0), (-1, 1)]
    }

    bottom = { # row, col
        'minus': [(0, 0), (0, 1), (0, 2), (0, 3)],
        'plus': [(-1, 0), (0, 1), (-1, 2)],
        'l': [(0, 0), (0, 1), (0, 2)],
        'I': [(0, 0)],
        'square': [(0, 0), (0, 1)]
    }
    rights = { # row, col
        'minus': [(0, 3)],
        'plus': [(-2, 1), (-1, 2), (0, 1)],
        'l': [(0, 2), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 1), (-1, 1)]
    }
    lefts = { # row, col
        'minus': [(0, 0)],
        'plus': [(-2, 1), (-1, 0), (0, 1)],
        'l': [(0, 0), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 0), (-1, 0)]
    }

    nm = 0
    for t in range(2022):
        name = names[t % 5]
        leftmost = 2
        botmost = 0
        while True:
            if inp[nm] == '>':
                leftmost += right(board, rights[name], leftmost, botmost)
            elif inp[nm] == '<':
                leftmost -= left(board, lefts[name], leftmost, botmost)
            nm = (nm + 1) % len(inp)
            if touching(board, bottom[name], leftmost, botmost):
                break
            botmost += 1
        update(board, ss[name], leftmost, botmost)
        h = highest(board)
        if h < 4:
            board = [['.' for _ in range(7)] for _ in range(4 - h)] + board

    return len(board) - highest(board)


def part2(inp):
    board = [['.' for _ in range(7)] for _ in range(4)]

    names = ['minus', 'plus', 'l', 'I', 'square']

    ss = { # row, col
        'minus': [(0, 0), (0, 1), (0, 2), (0, 3)],
        'plus': [(-1, 0), (0, 1), (-1, 1), (-1, 2), (-2, 1)],
        'l': [(0, 0), (0, 1), (0, 2), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 0), (0, 1), (-1, 0), (-1, 1)]
    }

    bottom = { # row, col
        'minus': [(0, 0), (0, 1), (0, 2), (0, 3)],
        'plus': [(-1, 0), (0, 1), (-1, 2)],
        'l': [(0, 0), (0, 1), (0, 2)],
        'I': [(0, 0)],
        'square': [(0, 0), (0, 1)]
    }
    rights = { # row, col
        'minus': [(0, 3)],
        'plus': [(-2, 1), (-1, 2), (0, 1)],
        'l': [(0, 2), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 1), (-1, 1)]
    }
    lefts = { # row, col
        'minus': [(0, 0)],
        'plus': [(-2, 1), (-1, 0), (0, 1)],
        'l': [(0, 0), (-1, 2), (-2, 2)],
        'I': [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
        'square': [(0, 0), (-1, 0)]
    }
    M = 1000000000000

    nm = 0
    seen = {} # (pattern, nm, name) -> move
    heights = {}
    for t in range(2022):
        name = names[t % 5]
        leftmost = 2
        botmost = 0

        heights[t] = len(board) - highest(board)
        rows = ''
        for row in board[4:10]:
            rows += str(row)
        if (rows, nm, name) in seen:
            tseen = seen[(rows, nm, name)]
            repeat = t - tseen
            hrepeat = heights[t] - heights[tseen]
            return ((M - tseen) // repeat) * hrepeat + heights[tseen + ((M - tseen) % repeat)]
        seen[(rows, nm, name)] = t

        while True:
            if inp[nm] == '>':
                leftmost += right(board, rights[name], leftmost, botmost)
            elif inp[nm] == '<':
                leftmost -= left(board, lefts[name], leftmost, botmost)
            nm = (nm + 1) % len(inp)
            if touching(board, bottom[name], leftmost, botmost):
                break
            botmost += 1

        update(board, ss[name], leftmost, botmost)
        h = highest(board)
        if h < 4:
            board = [['.' for _ in range(7)] for _ in range(4 - h)] + board


if __name__ == '__main__':
    main()

