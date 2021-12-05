import os
dir_path = os.path.dirname(os.path.realpath(__file__))

from itertools import permutations


def part1(inp):
    cities = set()
    paths = {}
    for (c1, c2), d in inp:
        paths[(c1, c2)] = d
        paths[(c2, c1)] = d
        cities.add(c1)
        cities.add(c2)

    best = 99999
    for perm in permutations(cities):
        dist = 0
        for cs in zip(perm, perm[1:]):
            if cs in paths:
                dist += paths[cs]
            else:
                dist = -1
                break
        
        if dist > 0 and dist < best:
            best = dist
    
    return best


def part2(inp):
    cities = set()
    paths = {}
    for (c1, c2), d in inp:
        paths[(c1, c2)] = d
        paths[(c2, c1)] = d
        cities.add(c1)
        cities.add(c2)

    best = 0
    for perm in permutations(cities):
        dist = 0
        for cs in zip(perm, perm[1:]):
            if cs in paths:
                dist += paths[cs]
            else:
                dist = -1
                break
        
        if dist > 0 and dist > best:
            best = dist
    
    return best


def main():
    with open(f'{dir_path}/../../inputs/day09/input') as f:
        inp = list(map(lambda x: (tuple(x.split(' = ')[0].split(' to ')), int(x.split(' = ')[1])), f.read().strip().split('\n')))
    
    print(inp)
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

