import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    a, b = line.split('-')
    return int(a), int(b)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day04/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day04/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    count = 0
    for p in range(inp[0], inp[1] + 1):
        p = str(p)
        twosame = any(a == b for a, b in zip(p, p[1:]))
        inc = all(a <= b for a, b in zip(p, p[1:]))
        if twosame and inc:
            count +=1
    return count
            

def part2(inp):
    count = 0
    for p in range(inp[0], inp[1] + 1):
        p = str(p)
        inc = all(a <= b for a, b in zip(p, p[1:]))
        p = '-1' + p + '-1'
        twosame = any(a != b == c != d for a, b, c, d in zip(p, p[1:], p[2:], p[3:]))
        if twosame and inc:
            count +=1
    return count


if __name__ == '__main__':
    main()

