import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from random import random


def part1(inp):
    elves = [i + 1 for i in range(inp)]
    
    while len(elves) > 1:
        if len(elves) % 2 == 1:
            new_elves = [e for i, e in enumerate(elves[2:]) if i % 2 == 0]
        else:
            new_elves = [e for i, e in enumerate(elves) if i % 2 == 0]

        elves = new_elves

    return elves[0]


def part2(inp):
    elves = [i + 1 for i in range(inp)]
    cur = len(elves) // 2
    move2 = True
    left = len(elves)
    while left > 1:
        elves[cur] = False
        left -= 1
        while not elves[cur]:
            cur = (cur + 1) % len(elves)
        if move2:
            cur = (cur + 1) % len(elves)
            while not elves[cur]:
                cur = (cur + 1) % len(elves)

        move2 = not move2

    return [c for c in elves if c][0]


def main():
    with open(f'{dir_path}/../../inputs/day19/input') as f:
        inp = int(list(map(lambda x: x, f.read().strip().split('\n')))[0])

    print(inp)
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

