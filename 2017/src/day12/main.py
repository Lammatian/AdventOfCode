import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def part1(inp):
    connected = defaultdict(set)
    for s, e in inp:
        connected[s].update(e)
        for elt in e:
            connected[elt].add(s)
    seen = set()
    q = [0]
    while q:
        curr = q.pop(0)
        seen.add(curr)
        for c in connected[curr]:
            if c not in seen:
                q.append(c)
    return len(seen)


def part2(inp):
    connected = defaultdict(set)
    for s, e in inp:
        connected[s].update(e)
        for elt in e:
            connected[elt].add(s)

    groups = 0
    seen = set()
    for i in range(0, max(connected.keys())):
        if i in seen:
            continue
        groups += 1
        q = [i]
        while q:
            curr = q.pop(0)
            seen.add(curr)
            for c in connected[curr]:
                if c not in seen:
                    q.append(c)
    return groups


def parse(line):
    line = line.split(' <-> ')
    return [int(line[0]), [int(l) for l in line[1].split(', ')]]


def main():
    with open(f'{dir_path}/../../inputs/day12/input') as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

