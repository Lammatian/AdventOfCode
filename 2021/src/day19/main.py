import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import product, combinations, permutations
from numpy import subtract, add
from collections import defaultdict


def orientations(beacons):
    # There are 48 different orientations so some repeat lol
    for a, b, c in permutations((0, 1, 2), 3):
        for mx, my, mz in product([-1, 1], repeat=3):
            orientation = []
            for x, y, z in beacons:
                p = (mx * x, my * y, mz * z)
                orientation += [(p[a], p[b], p[c])]
            yield orientation


def overlap(inp):
    coords = {0: (0, 0, 0)}
    bs = {0: inp[0]}
    queue = [0]
    beacons = set(inp[0])
    orients = {
        i: list(orientations(inp[i])) for i in range(len(inp))
    }
    while queue:
        cur = queue.pop(0)
        for i in range(len(inp)):
            if i in coords:
                continue

            diffs = defaultdict(int)
            intersect = False
            for bs2 in orients[i]:
                for b1 in bs[cur]:
                    for b2 in bs2: 
                        diff = tuple(subtract(b1, b2))
                        diffs[diff] += 1
                        if diffs[diff] >= 12:
                            # (cur, i) intersect, adjust i's position
                            coords[i] = add(coords[cur], diff)
                            if i not in queue:
                                queue.append(i)
                            # Save oriented beacon positions
                            bs[i] = bs2
                            # Save true beacon positions
                            beacons.update(list(map(lambda x: tuple(add(coords[i], x)), bs2)))
                            intersect = True
                            break
                    if intersect:
                        break
                if intersect:
                    break
                     
    return beacons, coords


def part1(beacons):
    return len(beacons)


def part2(coords):
    dist = 0
    for i in range(len(coords)):
        for j in range(i + 1, len(coords)):
            dist = max(dist, sum(map(abs, subtract(coords[i], coords[j]))))
                     
    return dist


def main():
    with open(f'{dir_path}/../../inputs/day19/input') as f:
        inp = f.read().strip().split('\n\n')
        inp = list(map(lambda x: x.split('\n')[1:], inp))
        inp = list(map(lambda x: list(map(lambda y: tuple(map(int, y.split(','))), x)), inp))

    print(inp)
    
    beacons, coords = overlap(inp)
    print(part1(beacons))
    print(part2(coords))


if __name__ == '__main__':
    main()

