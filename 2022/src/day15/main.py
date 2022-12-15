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
    _, a, b = line.split('x=')
    a = a.split(', y=')
    a[1] = int(a[1][:a[1].index(':')])
    a[0] = int(a[0])
    b = b.split(', y=')
    b[0] = int(b[0])
    b[1] = int(b[1])
    return (a[1], a[0]), (b[1], b[0])


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day15/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day15/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    tr = 2000000
    positions = set()
    for (sr, sc), (br, bc) in inp:
        D = abs(sr - br) + abs(sc - bc)
        # Find all positions where cannot exist in ty = 2000000
        dc = D - abs(tr - sr)
        if dc < 0:
            continue
        for c in range(sc - dc, sc + dc + 1):
            positions.add(c)
    for _, b in inp:
        if b[0] == tr and b[1] in positions:
            positions.remove(b[1])
    return len(positions)


def part2(inp):
    max_ = 4000000
    DB = []
    for (sr, sc), (br, bc) in inp:
        db = abs(sr - br) + abs(sc - bc)
        DB.append(db)
    for r in range(max_):
        c = 0
        while c < max_:
            for (sr, sc), (br, bc) in inp:
                db = abs(sr - br) + abs(sc - bc)
                dc = abs(r - sr) + abs(c - sc)
                if dc <= db:
                    c = db - abs(r - sr) + sc + 1
                    break
            else:
                return max_ * c + r


if __name__ == '__main__':
    main()

