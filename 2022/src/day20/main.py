import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *
from copy import deepcopy
from random import randint


def parse(line):
    return int(line)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day20/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day20/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    N = len(inp)
    M = []
    c = defaultdict(int)
    for n in inp:
        M.append((n, c[n]))
        c[n] += 1

    tomove = M[:]

    for m, c in tomove:
        i = M.index((m, c))
        for _ in range(abs(m)):
            if m < 0:
                M[(i - 1) % N], M[i] = M[i], M[(i - 1) % N]
                i = (i - 1) % N
            else:
                M[i], M[(i + 1) % N] = M[(i + 1) % N], M[i]
                i = (i + 1) % N
    z = M.index((0, 0))
    res = 0
    for x in [1000, 2000, 3000]:
        res += M[(z + x) % N][0]
    return res


def part2(inp):
    N = len(inp)
    M = []
    c = defaultdict(int)
    for n in inp:
        M.append((n * 811589153, c[n]))
        c[n] += 1

    tomove = M[:]
    for _ in range(10):
        for m, c in tomove:
            moves = m % N
            if m < 0:
                moves -= N
            moves = abs(m) % (N-1) * (-1 if m < 0 else 1)
            i = M.index((m, c))
            for _ in range(abs(moves)):
                if moves < 0:
                    M[(i - 1) % N], M[i] = M[i], M[(i - 1) % N]
                    i = (i - 1) % N
                else:
                    M[i], M[(i + 1) % N] = M[(i + 1) % N], M[i]
                    i = (i + 1) % N
    z = M.index((0, 0))
    res = 0
    for x in [1000, 2000, 3000]:
        res += M[(z + x) % N][0]
    return res


if __name__ == '__main__':
    main()

