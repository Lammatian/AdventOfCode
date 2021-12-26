import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import permutations
from collections import defaultdict


def part1(inp):
    rules = defaultdict(dict)
    for a, v, b in inp:
        rules[a][b] = v

    names = list(set(x[0] for x in inp))

    best = -1e9
    bperm = None
    for p in permutations(names[1:]):
        perm = [names[0]] + list(p)
        cur = 0
        for i in range(len(perm)):
            n1 = perm[i]
            n2 = perm[(i + 1) % len(perm)]
            cur += rules[n1][n2] 
            cur += rules[n2][n1]

        if cur > best:
            best = max(best, cur)
            bperm = perm

    return best, bperm


def part2(inp):
    names = list(set(x[0] for x in inp))
    for name in names:
        inp.append(('me', 0, name))
        inp.append((name, 0, 'me'))

    names += ['me']
    rules = defaultdict(dict)
    for a, v, b in inp:
        rules[a][b] = v

    best = -1e9
    bperm = None
    for p in permutations(names[1:]):
        perm = [names[0]] + list(p)
        cur = 0
        for i in range(len(perm)):
            n1 = perm[i]
            n2 = perm[(i + 1) % len(perm)]
            cur += rules[n1][n2] 
            cur += rules[n2][n1]

        if cur > best:
            best = max(best, cur)
            bperm = perm

    return best, bperm


def main():
    with open(f'{dir_path}/../../inputs/day13/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    inp = [(x[0], int(x[3]) if x[2] == 'gain' else -int(x[3]), x[-1][:-1]) for x in inp]
    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

