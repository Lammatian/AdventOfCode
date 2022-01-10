import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from hashlib import md5
from random import random


def part1(inp):
    queue = [(inp, (0, 0), 0)]

    while queue:
        path, (r, c), steps = queue.pop(0)
        h = md5(path.encode()).hexdigest()

        dr = [-1, 1, 0, 0]
        dc = [0, 0, -1, 1]
        dd = ['U', 'D', 'L', 'R']

        for i, C in enumerate(h[:4]):
            if C in 'bcdef':
                r_ = r + dr[i]
                c_ = c + dc[i]
                if r_ < 0 or r_ > 3 or c_ < 0 or c_ > 3:
                    continue
                if (r_, c_) == (3, 3):
                    return path[len(inp):] + dd[i]
                queue.append((path + dd[i], (r_, c_), steps + 1))
        

def part2(inp):
    queue = [(inp, (0, 0), 0)]
    longest = 0

    while queue:
        path, (r, c), steps = queue.pop(0)
        h = md5(path.encode()).hexdigest()

        dr = [-1, 1, 0, 0]
        dc = [0, 0, -1, 1]
        dd = ['U', 'D', 'L', 'R']

        for i, C in enumerate(h[:4]):
            if C in 'bcdef':
                r_ = r + dr[i]
                c_ = c + dc[i]
                if r_ < 0 or r_ > 3 or c_ < 0 or c_ > 3:
                    continue
                if (r_, c_) == (3, 3):
                    longest = max(longest, steps + 1)
                    continue
                queue.append((path + dd[i], (r_, c_), steps + 1))

    return longest


def main():
    with open(f'{dir_path}/../../inputs/day17/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

