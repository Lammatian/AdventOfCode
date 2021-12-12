import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def part1rec(inp, visited, curr):
    total = 0
    avn = [n
        for c, n in inp
        if c == curr and n not in visited and n != 'start'
    ]
    avn += [n
        for n, c in inp
        if c == curr and n not in visited and n != 'start'
    ]
    for nex in avn:
        if nex.isupper():
            total += part1rec(inp, visited, nex)
        elif nex == 'end':
            total += 1
        elif nex not in visited:
            visited.add(nex)
            total += part1rec(inp, visited, nex)
            visited.remove(nex)

    return total


def part1(inp):
    return part1rec(inp, set(), 'start')


def part2rec(inp, visited, path, small):
    curr = path.split(',')[-1]
    total = set()
    avn = [n
        for c, n in inp
        if c == curr and n != 'start'
    ]
    avn += [n
        for n, c in inp
        if c == curr and n != 'start'
    ]
    for nex in avn:
        if nex.isupper():
            total.update(part2rec(inp, visited, path + "," + nex, small))
        elif nex == 'end':
            total.update(set([path + ',end']))
        elif visited[nex] == 0:
            visited[nex] += 1
            total.update(part2rec(inp, visited, path + "," + nex, small))
            visited[nex] -= 1
        elif nex == small and visited[nex] == 1:
            visited[nex] += 1
            total.update(part2rec(inp, visited, path + "," + nex, small))
            visited[nex] -= 1

    return total


def part2(inp):
    smalls = [a for a, b in inp if a.islower() and a not in ['start', 'end']]
    smalls += [b for a, b in inp if b.islower() and b not in ['start', 'end']]
    paths = set()
    for small in set(smalls):
        paths.update(part2rec(inp, defaultdict(int), 'start', small))
        
    return len(paths)

def main():
    with open(f'{dir_path}/../../inputs/day12/input') as f:
        inp = list(map(lambda x: x.split('-'), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

