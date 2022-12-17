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

    print(part1_set(deepcopy(inp)))
    print(part2_set(deepcopy(inp)))
    print(part1_grid(deepcopy(inp)))
    print(part2_grid(deepcopy(inp)))


def get_piece(s, y):
    if s == 0:
        return set([(2, y), (3, y), (4, y), (5, y)])
    elif s == 1:
        return set([(2, y+1), (3, y), (3, y+1), (3, y+2), (4, y+1)])
    elif s == 2:
        return set([(2, y), (3, y), (4, y), (4, y+1), (4, y+2)])
    elif s == 3:
        return set([(2, y), (2, y+1), (2, y+2), (2, y+3)])
    elif s == 4:
        return set([(2, y), (2, y+1), (3, y), (3, y+1)])
    else:
        assert False


def move_right(piece):
    if any(x+1 >= 7 for x, _ in piece):
        return piece
    return set([(x+1, y) for x, y in piece])


def move_left(piece):
    if any(x-1 < 0 for x, _ in piece):
        return piece
    return set([(x-1, y) for x, y in piece])


def move_down(piece):
    return set([(x, y-1) for x, y in piece])


def move_up(piece):
    return set([(x, y+1) for x, y in piece])


def show(B):
    maxy = max(y for x, y in B)
    for y in range(maxy, 0, -1):
        for x in range(7):
            if (x, y) in B:
                print('#', end='')
            else:
                print('.', end='')
        print()


def part1_set(inp):
    B = set([(x, 0) for x in range(7)])
    nm = 0
    
    for t in range(2022):
        top = max(y for _, y in B) + 4
        piece = get_piece(t%5, top)
        while True:
            if inp[nm] == '>':
                piece = move_right(piece)
                if piece & B:
                    piece = move_left(piece)
            elif inp[nm] == '<':
                piece = move_left(piece)
                if piece & B:
                    piece = move_right(piece)
            nm = (nm + 1) % len(inp)
            piece = move_down(piece)
            if piece & B:
                piece = move_up(piece)
                break
        B |= piece

    return max(y for _, y in B)


def signature(B):
    maxy = max(y for _, y in B)
    return frozenset([(x, maxy-y) for x, y in B if maxy - y <= 6])


def part2_set(inp):
    B = set([(x, 0) for x in range(7)])
    nm = 0
    M = 1000000000000
    
    t = 0
    seen = {}
    top = 0
    added_height = 0
    added = False
    while t < M:
        piece = get_piece(t%5, top+4)
        while True:
            if inp[nm] == '>':
                piece = move_right(piece)
                if piece & B:
                    piece = move_left(piece)
            elif inp[nm] == '<':
                piece = move_left(piece)
                if piece & B:
                    piece = move_right(piece)
            nm = (nm + 1) % len(inp)
            piece = move_down(piece)
            if piece & B:
                piece = move_up(piece)
                B |= piece
                top = max(y for _, y in B)
                break

        sig = (nm, t%5, signature(B))
        if sig in seen and not added:
            oldt, oldy = seen[sig]
            dy = top - oldy
            dt = t - oldt
            amt = (M - t) // dt
            added_height = dy * amt
            t += dt * amt
            added = True

        seen[sig] = (t, top)
        t += 1

    return top + added_height


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


def part1_grid(inp):
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


def part2_grid(inp):
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

