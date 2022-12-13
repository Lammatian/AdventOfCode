import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *
from copy import deepcopy
from functools import cmp_to_key


def parse(line):
    a, b = line.split('\n')
    return [eval(a), eval(b)]


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day13/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day13/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def rec(x, y):
    inright = True
    if type(x) == type([]) and type(y) == type([]):
        for x_, y_ in zip(x, y):
            inright = rec(x_, y_)
            if inright == -1:
                return -1
            if inright == 1:
                return 1
        return 1 if len(x) < len(y) else (0 if len(x) == len(y) else -1)
    elif type(x) == type([]) and type(y) == type(5):
        return rec(x, [y])
    elif type(x) == type(5) and type(y) == type([]):
        return rec([x], y)
    elif type(x) == type(5) and type(y) == type(5):
        return 1 if x < y else (0 if x == y else -1)


def part1(inp):
    res = 0
    for i, (a, b) in enumerate(inp):
        if rec(a, b) == 1:
            res += i + 1
    return res


def part2(inp):
    packets = []
    for a, b in inp:
        packets.append(a)
        packets.append(b)
    packets.append([[2]])
    packets.append([[6]])
    packets = sorted(packets, key=cmp_to_key(rec), reverse=True)
    return (packets.index([[2]]) + 1) * (packets.index([[6]]) + 1)
    


if __name__ == '__main__':
    main()

