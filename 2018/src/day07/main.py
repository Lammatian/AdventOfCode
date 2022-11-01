import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day07/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day07/input'

    with open(filepath) as f:
        inp = list(map(lambda x: [x.split()[1], x.split()[-3]], f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    reqs = defaultdict(set)
    for s, e in inp:
        reqs[e].add(s)
        if s not in reqs:
            reqs[s] = set()

    finished = []
    while True:
        n = list(sorted(k for k, v in reqs.items() if k not in finished and v == set()))
        if len(n) == 0:
            break
        n = n[0]
        for _, v in reqs.items():
            if n in v:
                v.remove(n)
        finished += [n]
    return ''.join(finished)


def part2(inp):
    reqs = defaultdict(set)
    for s, e in inp:
        reqs[e].add(s)
        if s not in reqs:
            reqs[s] = set()

    finished = []
    ELVES = 5
    ORD_ADD = 61
    n = list(sorted(k for k, v in reqs.items() if k not in finished and v == set()))[:ELVES]
    work_left = {k: ord(k) - ord('A') + ORD_ADD for k in n}
    seconds = 0
    print(work_left)
    while work_left:
        min_val = min(work_left.values())
        for k, v in work_left.items():
            work_left[k] -= min_val
        seconds += min_val

        popped = [k for k, v in work_left.items() if v == 0]
        for k in popped:
            del work_left[k]
            for _, v in reqs.items():
                if k in v:
                    v.remove(k)
        finished += sorted(popped)
        n = list(sorted(k for k, v in reqs.items() if k not in finished and k not in work_left and v == set()))[:ELVES - len(work_left)]
        for k in n:
            work_left[k] = ord(k) - ord('A') + ORD_ADD
        print(finished)
        print(work_left)
        print(reqs)

    return seconds


if __name__ == '__main__':
    main()

