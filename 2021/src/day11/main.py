import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from copy import deepcopy


def part1(inp):
    result = 0
    for i in range(100):
        flashes = []
        visited = set()
        for y, row in enumerate(inp):
            for x, val in enumerate(row):
                if inp[y][x] == 9:
                    inp[y][x] = 0
                    flashes.append((x, y))
                else:
                    inp[y][x] += 1

        while flashes:
            x, y = flashes.pop()
            visited.add((x, y))

            for i in [-1, 0, 1]:
                for j in [-1, 0, 1]:
                    xx = x + i
                    yy = y + j
                    if 0 <= xx < len(inp[0]) and 0 <= yy < len(inp) and (xx, yy) not in visited :
                        if inp[yy][xx] == 9:
                            inp[yy][xx] = 0
                            flashes.append((xx, yy))
                        else:
                            inp[yy][xx] += 1
        
        for x, y in visited:
            inp[y][x] = 0

        result += len(visited)

    return result


def part2(inp):
    result = 0
    for r in range(10000000):
        flashes = []
        visited = set()
        for y, row in enumerate(inp):
            for x, val in enumerate(row):
                if inp[y][x] == 9:
                    inp[y][x] = 0
                    flashes.append((x, y))
                else:
                    inp[y][x] += 1

        while flashes:
            x, y = flashes.pop()
            visited.add((x, y))

            for i in [-1, 0, 1]:
                for j in [-1, 0, 1]:
                    xx = x + i
                    yy = y + j
                    if 0 <= xx < len(inp[0]) and 0 <= yy < len(inp) and (xx, yy) not in visited :
                        if inp[yy][xx] == 9:
                            inp[yy][xx] = 0
                            flashes.append((xx, yy))
                        else:
                            inp[yy][xx] += 1
        
        for x, y in visited:
            inp[y][x] = 0

        if len(visited) == len(inp) * len(inp[0]):
            return r


def main():
    with open(f'{dir_path}/../../inputs/day11/sample') as f:
        inp = list(map(lambda x: list(map(int, list(x))), f.read().strip().split('\n')))

    print(inp)
    print(part1(deepcopy(inp[:])))
    print(part2(deepcopy(inp[:])))


if __name__ == '__main__':
    main()

