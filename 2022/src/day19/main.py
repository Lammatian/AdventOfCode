import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce, cache
from collections import Counter, defaultdict, deque
import numpy as np
from pyutils import *
from copy import deepcopy
sys.setrecursionlimit(1500)

def parse(line):
    parts = list(map(lambda x: x.split(), line.split('.')))
    return int(parts[0][-2]), int(parts[1][-2]), (int(parts[2][-5]), int(parts[2][-2])), (int(parts[3][-5]), int(parts[3][-2]))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day19/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day19/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    print('C++ knows the answers')
    pass


def part2(inp):
    print('C++ knows the answers')
    pass


if __name__ == '__main__':
    main()

