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
    result, op = line.split(':')
    return result, op.split()


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day21/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day21/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def part1(inp):
    vals = {}
    for r, op in inp:
        if len(op) == 1:
            vals[r] = int(op[0])

    while 'root' not in vals:
        for r, op in inp:
            if len(op) > 1:
                a, o, b = op
                if a in vals and b in vals:
                    va = vals[a]
                    vb = vals[b]
                    if o == '+':
                        vals[r] = va + vb
                    if o == '-':
                        vals[r] = va - vb
                    if o == '*':
                        vals[r] = va * vb
                    if o == '/':
                        vals[r] = va // vb

    return vals['root']


def part2(inp):
    vals = {}
    s1, s2 = None, None
    for r, op in inp:
        if len(op) == 1:
            if r == 'humn':
                continue
            vals[r] = int(op[0])
        if r == 'root':
            s1 = op[0]
            s2 = op[2]

    # The MBS - manual binary search :D
    for t in range(int(3.555057453*10**12), int(3.5550575*10**12)):
        vals['humn'] = t
        V = deepcopy(vals)
        while not (s1 in V and s2 in V):
            for r, op in inp:
                if len(op) > 1:
                    a, o, b = op
                    if a in V and b in V:
                        va = V[a]
                        vb = V[b]
                        if o == '+':
                            V[r] = va + vb
                        if o == '-':
                            V[r] = va - vb
                        if o == '*':
                            V[r] = va * vb
                        if o == '/':
                            V[r] = va // vb
        if V[s1] == V[s2]:
            return t
        else:
            print(s1, s2, V[s1], V[s2], V[s1] - V[s2])


if __name__ == '__main__':
    main()

