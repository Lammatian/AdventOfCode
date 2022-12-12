import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from queue import PriorityQueue, Queue
from pyutils import *
from copy import deepcopy


def parse(line):
    return list(line)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day12/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day12/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    seen = set()
    R = len(inp)
    C = len(inp[0])
    q = []
    for r, row in enumerate(inp):
        for c, val in enumerate(row):
            if val == 'S':
                q.append((0, r, c))
                seen.add((r, c))
                inp[r][c] = 'a'

    while q:
        steps, r, c = q.pop(0)
        D = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        for dr, dc in D:
            rr = r + dr
            cc = c + dc
            if not (0 <= rr < R and 0 <= cc < C):
                continue
            if inp[r][c] in 'yz' and inp[rr][cc] == 'E':
                return steps + 1
            if not (ord(inp[rr][cc]) - ord(inp[r][c]) <= 1):
                continue
            if (rr, cc) in seen:
                continue
            q.append((steps + 1, rr, cc))
            seen.add((rr, cc))


def part2(inp):
    starts = []
    R = len(inp)
    C = len(inp[0])
    for r, row in enumerate(inp):
        for c, val in enumerate(row):
            if val == 'S':
                inp[r][c] = 'a'
    for r, row in enumerate(inp):
        for c, val in enumerate(row):
            if val in 'Sa':
                starts.append((r, c))

    best = 1e9
    for sr, sc in starts:
        seen = set()
        q = []
        seen.add((sr, sc))
        q.append((0, sr, sc))

        done = False
        while not done and q:
            steps, r, c = q.pop(0)
            D = [(1, 0), (-1, 0), (0, 1), (0, -1)]
            for dr, dc in D:
                rr = r + dr
                cc = c + dc
                if not (0 <= rr < R and 0 <= cc < C):
                    continue
                if inp[r][c] in 'yz' and inp[rr][cc] == 'E':
                    best = min(best, steps + 1)
                    done = True
                    break
                if not (ord(inp[rr][cc]) - ord(inp[r][c]) <= 1):
                    continue
                if (rr, cc) in seen:
                    continue
                q.append((steps + 1, rr, cc))
                seen.add((rr, cc))
    return best


if __name__ == '__main__':
    main()

