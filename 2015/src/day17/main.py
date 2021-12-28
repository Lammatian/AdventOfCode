import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import combinations


def part1(inp):
    count = 0
    for i in range(1, len(inp) + 1):
        for perm in combinations(inp, i):
            if sum(perm) == 150:
                count += 1
    return count


def part2(inp):
    for i in range(1, len(inp) + 1):
        count = 0
        for perm in combinations(inp, i):
            if sum(perm) == 150:
                count += 1
        if count > 0:
            return count


def main():
    with open(f'{dir_path}/../../inputs/day17/input') as f:
        inp = list(map(lambda x: int(x), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

