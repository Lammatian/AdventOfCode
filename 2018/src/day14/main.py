import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return int(line)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day14/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day14/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp))
    print(part2(inp))


def part1(inp):
    recipes = [3, 7]
    e1 = 0
    e2 = 1
    while len(recipes) < inp + 10:
        r1 = recipes[e1]
        r2 = recipes[e2]
        new = str(r1 + r2)
        for c in new:
            recipes.append(int(c))
        e1 = (e1 + r1 + 1) % len(recipes)
        e2 = (e2 + r2 + 1) % len(recipes)
    return ''.join(map(str, recipes[inp:inp+10]))


def part2(inp):
    recipes = "37"
    e1 = 0
    e2 = 1
    while recipes[-len(str(inp)):] != str(inp) and recipes[-len(str(inp)) - 1:-1] != str(inp):
        r1 = int(recipes[e1])
        r2 = int(recipes[e2])
        new = str(r1 + r2)
        recipes += new
        e1 = (e1 + r1 + 1) % len(recipes)
        e2 = (e2 + r2 + 1) % len(recipes)
    return recipes.find(str(inp))


if __name__ == '__main__':
    main()

