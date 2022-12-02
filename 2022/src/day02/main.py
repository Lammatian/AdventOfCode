import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line.split()


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day02/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day02/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    score = 0
    win = [['A', 'Y'], ['B', 'Z'], ['C', 'X']]
    draw = [['A', 'X'], ['B', 'Y'], ['C', 'Z']]
    for a, b in inp:
        if [a, b] in win:
            score += 6
        elif [a, b] in draw:
            score += 3
        score += ord(b) - ord('X') + 1
    return score


def part2(inp):
    score = 0
    res = {'X': 0, 'Y': 3, 'Z': 6}
    for a, b in inp:
        score += res[b]
        if b == 'Y':
            score += ord(a) - ord('A') + 1
        elif b == 'Z':
            if a == 'A':
                score += 2
            if a == 'B':
                score += 3
            if a == 'C':
                score += 1
        else:
            if a == 'A':
                score += 3
            if a == 'B':
                score += 1
            if a == 'C':
                score += 2


    return score


if __name__ == '__main__':
    main()

