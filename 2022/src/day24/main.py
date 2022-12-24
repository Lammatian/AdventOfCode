import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict, deque
import numpy as np
from pyutils import *
from copy import deepcopy


def parse(line):
    return line


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day24/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day24/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(solve(deepcopy(inp)))


vtod = {
    '<': (0, -1),
    '^': (-1, 0),
    '>': (0, 1),
    'v': (1, 0)
}


def board(B, R, C):
    """Helper for printing the board"""
    b = [['.' for _ in range(C)] for _ in range(R)]
    b[0] = ['#' for _ in range(C)]
    b[-1] = ['#' for _ in range(C)]
    b[0][1] = '.'
    b[-1][-2] = '.'
    for r in range(R):
        b[r][0] = '#'
        b[r][-1] = '#'
    for r, c, v in B:
        if b[r][c] == '.':
            b[r][c] = v
        elif b[r][c] in 'v><^':
            b[r][c] = '2'
        else:
            b[r][c] = str(int(b[r][c]) + 1)

    for row in b:
        print(''.join(row))


def go(start, end, t, T, R, C):
    q = deque()
    q.append((*start, t))
    seen = defaultdict(set)
    while q:
        r, c, t = q.popleft()
        if (r, c) in seen[t]:
            continue
        seen[t].add((r, c))

        for dr, dc in [(0, 0), (-1, 0), (0, -1), (1, 0), (0, 1)]:
            rr = r + dr
            cc = c + dc
            if (rr, cc) == end:
                return t + 1
            if not (rr, cc) == start and not (1 <= rr < R - 1 and 1 <= cc < C - 1):
                continue
            if (rr, cc) not in T[t + 1]:
                q.append((rr, cc, t + 1))

    assert(False)


def solve(inp):
    B = set()
    for r, row in enumerate(inp):
        for c, col in enumerate(row):
            if col in vtod.keys():
                B.add((r, c, col))

    R = len(inp)
    C = len(inp[0])
    T = [B]
    for _ in range(3000):
        newB = set()
        for r, c, v in B:
            dr, dc = vtod[v]
            rr = r + dr
            cc = c + dc
            if rr < 1:
                rr = R - 2
            elif rr >= R - 1:
                rr = 1
            elif cc < 1:
                cc = C - 2
            elif cc >= C - 1:
                cc = 1
            newB.add((rr, cc, v))
        T.append(set([(r, c) for r, c, _ in newB]))
        B = newB


    START = (0, 1)
    END = (R - 1, C - 2)
    t1 = go(START, END, 0, T, R, C)
    t2 = go(END, START, t1, T, R, C)
    t3 = go(START, END, t2, T, R, C)
    return t1, t3


if __name__ == '__main__':
    main()

