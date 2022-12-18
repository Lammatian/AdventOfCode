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
    return list(map(int, line.split(',')))


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day18/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day18/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


def surface_area(pts):
    res = 0
    for x, y, z in pts:
        res += 6
        for dx,dy,dz in [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0),(0,0,-1),(0,0,1)]:
            if (x + dx, y + dy, z + dz) in pts:
                res -= 1

    return res


def part1(inp):
    pts = set(map(tuple, inp))
    return surface_area(pts)


def part2(inp):
    pts = set(map(tuple, inp))

    maxx = max(x for x, _, _ in pts)
    minx = min(x for x, _, _ in pts)
    maxy = max(y for _, y, _ in pts)
    miny = min(y for _, y, _ in pts)
    maxz = max(z for _, _, z in pts)
    minz = min(z for _, _, z in pts)
    RX = range(minx, maxx+1)
    RY = range(miny, maxy+1)
    RZ = range(minz, maxz+1)
    D = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0),(0,0,-1),(0,0,1)]
    insides = set()
    for x in RX:
        for y in RY:
            for z in RZ:
                p = (x, y, z)
                if p in pts:
                    continue
                if p in insides:
                    continue
                q = [p]
                seen = set([p])
                internal = True
                while q and internal:
                    cx, cy, cz = q.pop()
                    for dx, dy, dz in D:
                        xx = cx + dx
                        yy = cy + dy
                        zz = cz + dz
                        n = (xx, yy, zz)
                        if xx not in RX or yy not in RY or zz not in RZ:
                            internal = False
                            break
                        if n in seen:
                            continue
                        if n in pts:
                            continue
                        q.append(n)
                        seen.add(n)

                if internal:
                    insides |= seen

    return surface_area(pts) - surface_area(insides)



if __name__ == '__main__':
    main()

