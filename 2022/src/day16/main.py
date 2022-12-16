import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict, deque
import numpy as np
from pyutils import *
from copy import deepcopy


def parse(line):
    s = line.split()
    to = []
    for x in s[::-1]:
        if 'valve' in x:
            break
        if ',' in x:
            to.append(x[:-1])
        else:
            to.append(x)
    return s[1], int(s[4].split('=')[1][:-1]), to


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day16/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day16/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def shortest(edges, f, t):
    q = deque([(f, 0)])
    seen = set()
    while q:
        curr, d = q.popleft()
        for n in edges[curr]:
            if n in seen:
                continue
            if n == t:
                return d + 1
            seen.add(n)
            q.append((n, d + 1))
    return -1


def run(paths, rates, allowed_valves, minutes):
    # BFS (curr, minute, curr_pressure, opened)
    best = 0
    q = deque([('AA', 0, 0, [])])
    p2 = defaultdict(list)
    for k, vs in paths.items():
        for (v, d) in vs:
            if v in allowed_valves:
                p2[k].append((v,d))
    while q:
        curr, m, p, o = q.popleft()
        best = max(best, p)
        for v, l in p2[curr]:
            if v in o:
                continue
            if m + l >= minutes:
                continue
            q.append((v, m + l + 1, p + (minutes-1-m-l)*rates[v], o + [v]))
    return best


def part1(inp):
    # Calculate lengths for each pair
    edges = defaultdict(list)
    for v, _, vs in inp:
        edges[v] += vs

    rates = {v: r for v, r, _ in inp}

    valves = [v for v, r, _ in inp if v == 'AA' or r != 0]
    paths = defaultdict(list)
    for v1, v2 in combinations(valves, 2):
        s = shortest(edges, v1, v2)
        paths[v1].append((v2, s))
        paths[v2].append((v1, s))

    return run(paths, rates, valves, 30)


def part2(inp):
    # Calculate lengths for each pair
    edges = defaultdict(list)
    for v, _, vs in inp:
        edges[v] += vs

    rates = {v: r for v, r, _ in inp}

    valves = [v for v, r, _ in inp if v == 'AA' or r != 0]
    paths = defaultdict(list)
    for v1, v2 in combinations(valves, 2):
        s = shortest(edges, v1, v2)
        paths[v1].append((v2, s))
        paths[v2].append((v1, s))

    best = 0
    seen = set()
    for i in range(1, len(paths)):
        for c in combinations(list(paths.keys()), i):
            s1 = set(c)
            seen.add(tuple(sorted(s1)))
            s2 = set(paths.keys()) - s1
            if tuple(sorted(s2)) in seen:
                continue
            res1 = run(paths, rates, s1, 26)
            res2 = run(paths, rates, s2, 26)
            best = max(best, res1 + res2)
    return best


if __name__ == '__main__':
    main()

