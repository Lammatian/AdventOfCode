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
    moves = []
    lastnum = ''
    for c in line:
        if not c.isdigit():
            moves.append(int(lastnum))
            moves.append(c)
            lastnum = ''
        else:
            lastnum += c
    if lastnum:
        moves.append(int(lastnum))
    return moves


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day22/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day22/input'

    with open(filepath) as f:
        lol = f.read().split('\n\n')
        board = lol[0].split('\n')
        moves = parse(lol[1].strip())

    print(board, moves)

    print(part1(deepcopy(board), deepcopy(moves)))
    print(part2(deepcopy(board), deepcopy(moves)))


def wrapleft(board, rr, cc):
    j = ''.join(board[rr])
    h = j.find('#')
    d = j.find('.')
    c = d if h == -1 else (h if d == -1 else min(d, h))
    return rr, c


def wraptop(board, rr, cc):
    for r, row in enumerate(board):
        if row[cc] in '#.':
            return r, cc
    assert(False)


def wrapright(board, rr, cc):
    j = ''.join(board[rr])
    h = j.rfind('#')
    d = j.rfind('.')
    c = d if h == -1 else (h if d == -1 else max(d, h))
    return rr, c


def wrapbot(board, rr, cc):
    for r in range(len(board) - 1, -1, -1):
        if cc < len(board[r]) and board[r][cc] in '#.':
            return r, cc
    assert(False)


def part1(board, moves):
    B = set()
    O = set()
    start = None
    for R, row in enumerate(board):
        for C, col in enumerate(row):
            if col == '.':
                if start is None:
                    start = (R, C)
                B.add((R, C))
            elif col == '#':
                O.add((R, C))

    r, c, = start
    D = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    d = 0
    T = {'L': -1, 'R': 1}
    W = {0: wrapleft, 1: wraptop, 2: wrapright, 3: wrapbot}
    path = []
    for m in moves: 
        if isinstance(m, str):
            d = (d + T[m]) % len(D)
            continue

        dr, dc = D[d]
        i = 0
        while i < m:
            path.append((r, c, d))
            rr = r + dr
            cc = c + dc
            if (rr, cc) in O:
                break
            elif (rr, cc) not in B:
                rr, cc = W[d](board, rr, cc)
            if (rr, cc) in O:
                break
            r = rr
            c = cc
            i += 1
        path.append((r, c, d))

    return 1000 * (r + 1) + 4 * (c + 1) + d


# Actual input
# For each face and each direction stores (next face, edge of that face, if coords need inversion)
# Inversion here means moving from e.g. row 1 to row 48 on the other face because of how they're connected
transitions = {
    1: {
        0: (2, 'L', False),
        1: (3, 'T', False),
        2: (4, 'L', True),
        3: (6, 'L', False)
    },
    2: {
        0: (5, 'R', True),
        1: (3, 'R', False),
        2: (1, 'R', False),
        3: (6, 'B', False)
    },
    3: {
        0: (2, 'B', False),
        1: (5, 'T', False),
        2: (4, 'T', False),
        3: (1, 'B', False)
    },
    4: {
        0: (5, 'L', False),
        1: (6, 'T', False),
        2: (1, 'L', True),
        3: (3, 'L', False)
    },
    5: {
        0: (2, 'R', True),
        1: (6, 'R', False),
        2: (4, 'R', False),
        3: (3, 'B', False)
    },
    6: {
        0: (5, 'B', False),
        1: (2, 'T', False),
        2: (1, 'T', False),
        3: (4, 'B', False)
    }
}
side_to_d = {
    'L': 0,
    'T': 1,
    'R': 2,
    'B': 3
}
d_to_side = {
    0: 'L',
    1: 'T',
    2: 'R',
    3: 'B'
}
D = [(0, 1), (1, 0), (0, -1), (-1, 0)]
SZ = 50
T = {'L': -1, 'R': 1}
# Top-left corner of each face
face_to_board = {
    1: [0, SZ],
    2: [0, 2*SZ],
    3: [SZ, SZ],
    4: [2*SZ, 0],
    5: [2*SZ, SZ],
    6: [3*SZ, 0],
}

# Example input
transitions_ = {
    1: {
        0: (6, 'R', True),
        1: (4, 'T', False),
        2: (3, 'T', False),
        3: (2, 'T', True)
    },
    2: {
        0: (3, 'L', False),
        1: (5, 'B', True),
        2: (6, 'B', True),
        3: (1, 'T', True)
    },
    3: {
        0: (4, 'L', False),
        1: (5, 'L', True),
        2: (2, 'R', False),
        3: (1, 'L', False)
    },
    4: {
        0: (6, 'T', True),
        1: (5, 'T', False),
        2: (3, 'R', False),
        3: (1, 'B', False)
    },
    5: {
        0: (6, 'L', False),
        1: (2, 'B', True),
        2: (3, 'B', True),
        3: (4, 'B', False)
    },
    6: {
        0: (1, 'R', True),
        1: (2, 'R', True),
        2: (5, 'R', False),
        3: (4, 'R', True)
    }
}
SZ_ = 4
DMAP_ = {
    1: [0, 2*SZ],
    2: [SZ, 0],
    3: [SZ, SZ],
    4: [SZ, 2*SZ],
    5: [2*SZ, 2*SZ],
    6: [2*SZ, 3*SZ]
}


def need_swap(from_side, to_side):
    # If we need to swap row with column
    # This is true only if we move from top/bot to left/right or vice-verse
    return (from_side in 'LR') != (to_side in 'LR')


def wrap_face(f, r, c, d):
    old_side = d_to_side[d]
    new_f, new_side, inv = transitions[f][d]
    new_d = side_to_d[new_side]
    ns = need_swap(old_side, new_side)
    if new_side == 'R':
        new_r = c if ns else r
        if inv:
            new_r = SZ - new_r - 1
        new_c = SZ - 1
    elif new_side == 'L':
        new_r = c if ns else r
        if inv:
            new_r = SZ - new_r - 1
        new_c = 0
    elif new_side == 'B':
        new_r = SZ - 1
        new_c = r if ns else c
        if inv:
            new_c = SZ - new_c - 1
    elif new_side == 'T':
        new_r = 0
        new_c = r if ns else c
        if inv:
            new_c = SZ - new_c - 1
    else:
        assert False

    return new_f, new_r, new_c, new_d 


def to_board(f, r, c):
    br, bc = face_to_board[f]
    return br + r, bc + c
    

def part2(board, moves):
    # Start on face 1, at (0, 0), facing right
    f, r, c, d = 1, 0, 0, 0
    path = []
    for m in moves: 
        if isinstance(m, str):
            d = (d + T[m]) % len(D)
            continue

        for _ in range(m):
            path.append((*to_board(f, r, c), d))
            dr, dc = D[d]
            rr = r + dr
            cc = c + dc
            dd = d
            ff = f
            if rr not in range(SZ) or cc not in range(SZ):
                ff, rr, cc, dd = wrap_face(f, rr, cc, d)

            br, bc = to_board(ff, rr, cc)
            if board[br][bc] == '#':
                break

            d, f, r, c = dd, ff, rr, cc
        else:
            path.append((*to_board(f, r, c), d))

    br, bc = to_board(f, r, c)
    return 1000 * (br + 1) + 4 * (bc + 1) + d


if __name__ == '__main__':
    main()

