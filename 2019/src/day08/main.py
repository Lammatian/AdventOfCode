import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day08/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day08/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    W = 25
    H = 6
    ct = 25
    sc = 0
    for i in range(0, len(inp), W * H):
        c = Counter(inp[i:i+W*H])
        z = c['0']
        if z < ct:
            ct = z
            sc = c['1'] * c['2']
    return sc


def part2(inp):
    W = 25
    H = 6
    layers = [inp[i:i+W*H] for i in range(0, len(inp), W*H)]
    res = ['?'] * (W * H)
    for layer in layers:
        for i, c in enumerate(layer):
            if res[i] != '?':
                continue
            if c == '2':
                continue
            elif c == '1':
                res[i] = '#'
            else:
                res[i] = ' '
    for i in range(6):
        print(''.join(res[25*i:25*(i+1)]))
    return res


if __name__ == '__main__':
    main()

