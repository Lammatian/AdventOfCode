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
        filepath = f'{dir_path}/../../inputs/day01/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day01/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    inp = list(inp)
    res = 0
    for a, b in zip(inp, inp[1:] + [inp[0]]):
        if a == b:
            res += int(a)
    return res


def part2(inp):
    inp = list(inp)
    res = 0
    for a, b in zip(inp, inp[len(inp)//2:] + inp[:len(inp)//2]):
        if a == b:
            res += int(a)
    return res


if __name__ == '__main__':
    main()

