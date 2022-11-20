import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return int(line.split()[0]), int(line.split()[-2])


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day09/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day09/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    l = CircularList()
    scores = [0] * inp[0]

    for i in range(1, inp[1]):
        player = i % inp[0]
        if i % 23 == 0:
            l.back(7)
            scores[player] += i
            scores[player] += l.remove()
            continue
        l.forward(2)
        l.insert(i)

    return max(scores)


def part2(inp):
    inp = [inp[0], 100 * inp[1]]
    l = CircularList()
    scores = [0] * inp[0]

    for i in range(1, inp[1]):
        player = i % inp[0]
        if i % 23 == 0:
            l.back(7)
            scores[player] += i
            scores[player] += l.remove()
            continue
        l.forward(2)
        l.insert(i)

    return max(scores)


if __name__ == '__main__':
    main()

