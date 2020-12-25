import sys
from collections import Counter
from typing import List


def prep(board: List[str]) -> set((int, int)):
    return set([(x, y, 0)
        for y, row in enumerate(board)
            for x, c in enumerate(row)
                if c == '#'
    ])


NEIGHBOURS = set([(1, 0), (-1, 0), (0, 1), (0, -1)])


def print_bugs_2d(bugs: set((int, int, int)), level: int = 0) -> None:
    board = []

    for _ in range(5):
        board.append(["."] * 5)

    for x, y, z in bugs:
        if z == level:
            board[y][x] = '#'

    print()
    for row in board:
        print(''.join(row))
    print()


def evolve(bugs: set((int, int, int))) -> set((int, int, int)):
    counts = Counter()

    for x, y, z in bugs:
        counts.update(
            (x + dx, y + dy, 0)
            for dx, dy in NEIGHBOURS
            if 0 <= x + dx <= 4 and 0 <= y + dy <= 4
        )

    return {
        cell
        for cell in counts
        if counts[cell] == 1 or (counts[cell] == 2 and cell not in bugs)
    }


def cell_diversity(x: int, y: int) -> int:
    return 2**(y*5 + x)


def sol1(bugs: set((int, int))):
    seen_states = set()

    while frozenset(bugs) not in seen_states:
        seen_states.add(frozenset(bugs))
        bugs = evolve(bugs)

    return sum([cell_diversity(x, y) for x, y, _ in bugs])


def neighbours_3d(x: int, y: int, z: int):
    neighbours_2d = [(x + dx, y + dy, z) for dx, dy in NEIGHBOURS]
    neighbours = []

    for xn, yn, zn in neighbours_2d:
        if xn < 0:
            neighbours.append((1, 2, zn - 1))
        elif xn > 4:
            neighbours.append((3, 2, zn - 1))
        elif yn < 0:
            neighbours.append((2, 1, zn - 1))
        elif yn > 4:
            neighbours.append((2, 3, zn - 1))
        elif xn == 2 and yn == 2:
            if x == 1:
                for ynn in range(5):
                    neighbours.append((0, ynn, zn + 1))
            elif x == 3:
                for ynn in range(5):
                    neighbours.append((4, ynn, zn + 1))
            elif y == 1:
                for xnn in range(5):
                    neighbours.append((xnn, 0, zn + 1))
            elif y == 3:
                for xnn in range(5):
                    neighbours.append((xnn, 4, zn + 1))
        else:
            neighbours.append((xn, yn, zn))

    return neighbours


def print_bugs_3d(bugs: set((int, int, int)), levels: List[int]) -> None:
    for level in levels:
        print("Level", level)
        print_bugs_2d(bugs, level)
        print()



def evolve_3d(bugs: set((int, int, int))):
    counts = Counter()

    for x, y, z in bugs:
        counts.update(
            neighbours_3d(x, y, z)
        )

    return {
        cell
        for cell in counts
        if counts[cell] == 1 or (counts[cell] == 2 and cell not in bugs)
    }


def sol2(bugs: set((int, int, int))):
    for _ in range(200):
        bugs = evolve_3d(bugs)

    return len(bugs)


def main():
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(sol1(prep(lines)))
    print(sol2(prep(lines)))


if __name__ == '__main__':
    main()