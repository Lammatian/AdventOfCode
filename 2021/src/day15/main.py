import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict
from heapq import heappush, heappop


def part1(inp):
    distances = defaultdict(lambda: -1)
    distances[(0, 0)] = 0
    current = [(0, 0)]
    Nx = len(inp[0])
    Ny = len(inp)
    Dx = [0, 1, 0, -1]
    Dy = [1, 0, -1, 0]

    while current:
        x, y = current.pop(0)

        for dx, dy in zip(Dx, Dy):
            nx = x + dx
            ny = y + dy
            if 0 <= nx < Nx and 0 <= ny < Ny:
                d = distances[(x, y)] + inp[ny][nx]
                if distances[(nx, ny)] < 0 or distances[(nx, ny)] > d:
                    distances[(nx, ny)] = d
                    current.append((nx, ny))

    return distances[(Nx - 1, Ny - 1)] 


def part2(inp):
    distances = defaultdict(lambda: -1)
    distances[(0, 0)] = 0
    current = [(0, (0, 0))]
    Nx = len(inp[0])
    Ny = len(inp)
    Dx = [0, 1, 0, -1]
    Dy = [1, 0, -1, 0]

    while current:
        d, (x, y) = heappop(current)

        for dx, dy in zip(Dx, Dy):
            nx = x + dx
            ny = y + dy

            if 0 <= nx < Nx and 0 <= ny < Ny:
                val = inp[ny][nx]
            else:
                val = inp[ny % Ny][nx % Ny] + (ny // Ny) + (nx // Nx)
                if val > 9:
                    val -= 9

            if 0 <= nx < 5 * Nx and 0 <= ny < 5 * Ny:
                nd = d + val
                if distances[(nx, ny)] < 0 or distances[(nx, ny)] > nd:
                    distances[(nx, ny)] = nd
                    heappush(current, (nd, (nx, ny)))

        if (5 * Nx - 1, 5 * Ny - 1) in distances:
            break

    return distances[(5 * Nx - 1, 5 * Ny - 1)] 


def main():
    with open(f'{dir_path}/../../inputs/day15/input') as f:
        inp = list(map(lambda x: list(map(int, list(x))), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

