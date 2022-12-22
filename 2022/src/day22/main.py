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
    V = {0: '>', 1: 'v', 2: '<', 3: '^'}
    path = []
    for m in moves: 
        if isinstance(m, str):
            #print('turning', t, 'direction', d, D[d])
            d = (d + T[m]) % len(D)
            continue

        dr, dc = D[d]
        i = 0
        while i < m:
            path.append((r, c, d))
            print(r, c)
            rr = r + dr
            cc = c + dc
            if (rr, cc) in O:
                break
            elif (rr, cc) not in B:
                rr, cc = W[d](board, rr, cc)
            if (rr, cc) in O:
                break
            #if rr < 0 or cc < 0 or rr >= len(board) or cc >= len(board[rr]) or board[rr][cc] == ' ':
            #    rr, cc = W[d](board, rr, cc)
            #if board[rr][cc] == '#':
            #    break
            r = rr
            c = cc
            i += 1
        path.append((r, c, d))
        print(r, c)

        #toprint = []
        #for R, row in enumerate(board):
        #    if r == R:
        #        toprint.append(''.join(row[:c] + 'X' + row[c+1:]))
        #    else:
        #        toprint.append(''.join(row))
        #for pr, pc, pd in path:
        #    if (pr, pc) == (r, c):
        #        continue
        #    toprint[pr] = list(toprint[pr])
        #    toprint[pr][pc] = V[pd]
        #    toprint[pr] = ''.join(toprint[pr])
        #for row in toprint:
        #    print(row)
        #print()
    print(moves[:100])
    toprint = []
    for R, row in enumerate(board):
        if r == R:
            toprint.append(''.join(row[:c] + 'X' + row[c+1:]))
        else:
            toprint.append(''.join(row))
    for pr, pc, pd in path:
        if (pr, pc) == (r, c):
            continue
        toprint[pr] = list(toprint[pr])
        toprint[pr][pc] = V[pd]
        toprint[pr] = ''.join(toprint[pr])
    for row in toprint:
        print(row)
    print()

    print(r, c)

    return 1000 * (r + 1) + 4 * (c + 1) + d


def part2(board, moves):
    pass


if __name__ == '__main__':
    main()

