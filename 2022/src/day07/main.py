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
        filepath = f'{dir_path}/../../inputs/day07/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day07/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    result = 0
    curdir = tuple()
    cursize = 0
    dirsizes = defaultdict(int)
    for line in inp:
        if line[1] == 'cd':
            if line[2] == '..':
                curdir = curdir[:-1]
            elif line[2] != '/':
                curdir = curdir + (line[2], )
        if line[0][0] in '123456789':
            size = int(line[0])
            dirsizes[curdir] += size
            for i in range(len(curdir)):
                dirsizes[curdir[:-i]] += size
            if size < 100000:
                cursize += size
    for _, v in dirsizes.items():
        if v < 100000:
            result += v
    return result



def part2(inp):
    curdir = tuple()
    cursize = 0
    dirsizes = defaultdict(int)
    for line in inp:
        if line[1] == 'cd':
            if line[2] == '..':
                curdir = curdir[:-1]
            elif line[2] != '/':
                curdir = curdir + (line[2], )
        if line[0][0] in '123456789':
            size = int(line[0])
            dirsizes[curdir] += size
            for i in range(len(curdir)):
                dirsizes[curdir[:-i]] += size
            if size < 100000:
                cursize += size
    used = dirsizes[()]
    unused = 70000000 - used
    smallest = 1e9
    for _, v in dirsizes.items():
        if unused + v >= 30000000:
            smallest = min(smallest, v)
    return smallest


if __name__ == '__main__':
    main()

