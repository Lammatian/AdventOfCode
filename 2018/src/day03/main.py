import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def parse(line):
    line = line.split()
    line[2] = list(map(int, line[2][:-1].split(',')))
    line[3] = list(map(int, line[3].split('x')))
    return (line[2], line[3])


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day03/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day03/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    board = []
    for _ in range(2000):
        board.append([0] * 2000)

    for (a, b), (c, d) in inp:
        for col in range(a, a + c):
            for row in range(b, b + d):
                board[row][col] += 1

    result = 0
    for row in board:
        result += sum(x > 1 for x in row)
    return result


def part2(inp):
    board = []
    for _ in range(2000):
        board.append([0] * 2000)

    for (a, b), (c, d) in inp:
        for col in range(a, a + c):
            for row in range(b, b + d):
                board[row][col] += 1
       
    for i, ((a, b), (c, d)) in enumerate(inp):
        broken = False
        for col in range(a, a + c):
            for row in range(b, b + d):
                if board[row][col] > 1:
                    broken = True
                    break
            if broken:
                break

        if not broken:
            return i + 1
        


if __name__ == '__main__':
    main()

