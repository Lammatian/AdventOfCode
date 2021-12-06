import os
from collections import defaultdict
dir_path = os.path.dirname(os.path.realpath(__file__))


def cycle(inp):
    new = 0
    for i in range(len(inp)):
        if inp[i] == 0:
            new += 1
            inp[i] = 6
        else:
            inp[i] -= 1

    for i in range(new):
        inp.append(8)

    return inp
    

def cycle2(cinp):
    c = defaultdict(int)
    for key, val in cinp.items():
        if key == 0:
            c[6] += val
            c[8] += val
        else:
            c[key - 1] += val

    return c


def part1(inp):
    for _ in range(80):
        inp = cycle(inp)

    return len(inp)


def part2(inp):
    cinp = defaultdict(int)

    for val in inp:
        cinp[val] += 1

    for _ in range(256):
        cinp = cycle2(cinp)

    return sum(cinp.values())


def main():
    with open(f'{dir_path}/../../inputs/day06/input') as f:
        inp = list(map(int, f.read().strip().split(',')))

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

