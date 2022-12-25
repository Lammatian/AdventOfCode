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
        filepath = f'{dir_path}/../../inputs/day25/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day25/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def fsnafu(s):
    val = 0
    mul = 1
    m = {'0': 0, '1': 1, '2': 2, '-': -1, '=': -2}
    for c in s[::-1]:
        val += mul * m[c]
        mul *= 5

    return val


def tosnafu(v):
    rm = {0: '0', 1: '1', 2: '2', -1: '-', -2: '='}
    res = ''
    nextmod = 0
    while v > 0:
        mod = v % 5 + nextmod
        if mod > 2:
            mod -= 5
            nextmod = 1
        else:
            nextmod = 0
        res = rm[mod] + res
        v //= 5
    return res


def part1(inp):
    result = sum([fsnafu(row) for row in inp])
    return tosnafu(result)


def part2(inp):
    return 'Christmas time'


if __name__ == '__main__':
    main()

