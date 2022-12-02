import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations, combinations_with_replacement
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return list(map(int, line.split(',')))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day02/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day02/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    pc = 0
    inp[1] = 12
    inp[2] = 2
    while inp[pc] != 99:
        op1, op2, res = 0, 0, 0
        if inp[pc] == 1 or inp[pc] == 2:
            op1 = inp[inp[pc + 1]]
            op2 = inp[inp[pc + 2]]
            res = inp[pc + 3]
        if inp[pc] == 1:
            inp[res] = op1 + op2
        if inp[pc] == 2:
            inp[res] = op1 * op2
        pc += 4
    return inp[0]


def part2(inp):
    store = inp[:]
    for a, b in product(range(100), repeat=2):
        pc = 0
        inp = store[:]
        inp[1] = a
        inp[2] = b
        while inp[pc] != 99:
            op1, op2, res = 0, 0, 0
            if inp[pc] == 1 or inp[pc] == 2:
                op1 = inp[inp[pc + 1]]
                op2 = inp[inp[pc + 2]]
                res = inp[pc + 3]
            if inp[pc] == 1:
                inp[res] = op1 + op2
            if inp[pc] == 2:
                inp[res] = op1 * op2
            pc += 4
        if inp[0] == 19690720:
            return 100 * a + b


if __name__ == '__main__':
    main()

