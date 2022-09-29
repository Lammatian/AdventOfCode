import os
from collections import Counter
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    rights = set()
    full = set()
    for v in inp:
        if len(v) == 2:
            rights.update(tuple(v[1]))
            full.add(v[0])
        else:
            full.add(v[0])
    return list(full - rights)[0]


def part2(inp):
    inp1 = []
    for row in inp:
        if len(row) == 2:
            inp1.append((row[0], ))
        else:
            inp1.append((row[0], row[2]))
    curr = part1(inp1)
    weights = {}
    edges = {}
    for row in inp:
        weights[row[0]] = row[1]
        if len(row) > 2:
            edges[row[0]] = row[2]
            
    def rec(curr, edges, weights):
        if curr not in edges:
            return weights[curr], False
        kids = edges[curr]
        kid_weights = {}
        for k in kids:
            w, ret = rec(k, edges, weights)
            if ret:
                return w, True
            kid_weights[k] = w
        cs = {}
        for k, v in kid_weights.items():
            if v in cs:
                cs[v] += 1
            else:
                cs[v] = 1
        if len(cs) > 1:
            for k, w in kid_weights.items():
                if cs[w] == 1:
                    # Odd one out, find diff
                    other_weight = list(set(cs.keys()) - set([w]))[0]
                    diff = w - other_weight
                    return weights[k] - diff, True
        w = weights[curr] + sum(kid_weights.values())
        return w, False

    return rec(curr, edges, weights)[0]


def parse(line):
    line = line.replace(',', '').split()
    if len(line) > 2:
        return (line[0], line[3:])
    else:
        return (line[0], )


def parse2(line):
    line = line.replace(',', '').split()
    if len(line) > 2:
        return (line[0], int(line[1][1:-1]), line[3:])
    else:
        return (line[0], int(line[1][1:-1]))


def main():
    with open(f'{dir_path}/../../inputs/day07/input') as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))

    with open(f'{dir_path}/../../inputs/day07/input') as f:
        inp = list(map(lambda x: parse2(x), f.read().strip().split('\n')))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

