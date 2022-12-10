import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line.split(',')


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day03/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day03/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


DIR = {
    'R': (1, 0),
    'L': (-1, 0),
    'U': (0, -1),
    'D': (0, 1),
}

def part1(inp):
    w1 = inp[0]
    w2 = inp[1]
    v1 = set()
    v2 = set()
    c1 = (0, 0)
    for w in w1:
        cr, cc = c1
        for _ in range(int(w[1:])):
            cr += DIR[w[0]][0]
            cc += DIR[w[0]][1]
            v1.add((cr, cc))
        c1 = (cr, cc)
            
    c2 = (0, 0)
    for w in w2:
        cr, cc = c2
        for _ in range(int(w[1:])):
            cr += DIR[w[0]][0]
            cc += DIR[w[0]][1]
            v2.add((cr, cc))
        c2 = (cr, cc)

    mindist = 1e9
    for r, c in v1 & v2:
        mindist = min(mindist, abs(r) + abs(c))
    return mindist


def part2(inp):
    w1 = inp[0]
    w2 = inp[1]
    v1 = {}
    v2 = {}
    s1 = 0
    s2 = 0
    c1 = (0, 0)
    for w in w1:
        cr, cc = c1
        for _ in range(int(w[1:])):
            cr += DIR[w[0]][0]
            cc += DIR[w[0]][1]
            s1 += 1
            v1[(cr, cc)] = s1
        c1 = (cr, cc)
            
    minsteps = 1e9
    c2 = (0, 0)
    for w in w2:
        cr, cc = c2
        for _ in range(int(w[1:])):
            cr += DIR[w[0]][0]
            cc += DIR[w[0]][1]
            s2 += 1
            v2[(cr, cc)] = s2
            if (cr, cc) in v1:
                minsteps = min(minsteps, s2 + v1[(cr, cc)])
        c2 = (cr, cc)

    return minsteps

if __name__ == '__main__':
    main()

