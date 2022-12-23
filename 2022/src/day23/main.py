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
    return list(line)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day23/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day23/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(solve(deepcopy(inp)))


def board(E):
    maxr = max(r for r, c in E)
    minr = min(r for r, c in E)
    maxc = max(c for r, c in E)
    minc = min(c for r, c in E)
    R = maxr - minr + 1
    C = maxc - minc + 1
    board = [list('.' * C) for _ in range(R)]
    for r, c in E:
        board[r - minr][c - minc] = '#'

    for row in board:
        print(''.join(row))
    print()


def solve(inp):
    E = set()
    for r, row in enumerate(inp):
        for c, col in enumerate(row):
            if col == '#':
                E.add((r, c))

    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    check = {
        (-1, 0): [(-1, x) for x in [-1, 0, 1]],
        (1, 0): [(1, x) for x in [-1, 0, 1]],
        (0, -1): [(x, -1) for x in [-1, 0, 1]],
        (0, 1): [(x, 1) for x in [-1, 0, 1]],
    }
    # dir index
    D = 0

    part1 = 0
    for t in range(100000):
        newE = set()
        proposed = {}
        prop_pos = defaultdict(int)
        notmoved = 0
        for r, c in E:
            safe = True
            for dr in [-1, 0, 1]:
                for dc in [-1, 0, 1]:
                    if (dr, dc) != (0, 0) and (r + dr, c + dc) in E:
                        safe = False
                        break
                if not safe:
                    break
            if safe:
                notmoved += 1
                proposed[(r, c)] = (r, c)
                prop_pos[(r, c)] += 1
                continue

            curd = D
            d = directions[curd]
            moved = False
            while not moved:
                for Cr, Cc in check[d]:
                    rr = r + Cr
                    cc = c + Cc
                    if (rr, cc) in E:
                        curd = (curd + 1) % len(directions)
                        d = directions[curd]
                        break
                else:
                    rr = r + d[0]
                    cc = c + d[1]
                    proposed[(r, c)] = (rr, cc)
                    prop_pos[(rr, cc)] += 1
                    moved = True

                if curd == D:
                    break
    
            if not moved:
                proposed[(r, c)] = (r, c)
                prop_pos[(r, c)] += 1
        D = (D + 1) % len(directions)
        if notmoved == len(E):
            return part1, t+1
        for (r, c), (rr, cc) in proposed.items():
            if prop_pos[(rr, cc)] == 1:
                newE.add((rr, cc))
            else:
                newE.add((r, c))
        assert(len(newE) == len(E))
        E = newE

        if t == 9:
            maxr = max(r for r, c in E)
            minr = min(r for r, c in E)
            maxc = max(c for r, c in E)
            minc = min(c for r, c in E)
            R = maxr - minr + 1
            C = maxc - minc + 1
            part1 = R*C - len(E)

    return -1


if __name__ == '__main__':
    main()

