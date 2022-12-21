import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *
from copy import deepcopy
from z3 import Solver, sat, Real


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
    print(part2_z3(deepcopy(inp)))
    print(part2_binsearch(deepcopy(inp)))


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


def part2_z3(inp):
    s = Solver()
    for r, op in inp:
        if r == 'humn':
            # We're solving for humn
            continue
        elif len(op) == 1:
            v = Real(r)
            s.add(v == int(op[0]))
        elif r == 'root':
            v1 = Real(op[0])
            v2 = Real(op[2])
            s.add(v1 == v2)
        else:
            v1 = Real(op[0])
            v2 = Real(op[2])
            vr = Real(r)
            if op[1] == '+':
                s.add(vr == v1 + v2)
            if op[1] == '-':
                s.add(vr == v1 - v2)
            if op[1] == '*':
                s.add(vr == v1 * v2)
            if op[1] == '/':
                s.add(vr == v1 / v2)

    if s.check() == sat:
        m = s.model()
        for d in m:
            if str(d) == 'humn':
                return m[d]
    else:
        return -1


def part2_binsearch(inp):
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

    # Binary search with manually picked values for min_/max_
    # Floor division fucks this solution up a bit so it returns a value
    # off by 1 if max_/min_ are not set right 💁
    max_ = 3 * 10**13
    min_ = 0
    while max_ > min_:
        t = (max_ + min_) // 2
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
            if V[s1] - V[s2] > 0:
                min_ = t + 1
            else:
                max_ = t - 1

    return None


if __name__ == '__main__':
    main()

