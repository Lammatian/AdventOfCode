import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line.split()


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day10/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day10/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    cycle = 0
    X = 1
    S = 0
    intr = [20, 60, 100, 140, 180, 220]
    for ins in inp:
        if len(ins) == 2:
            if cycle + 1 in intr:
                S += X * (cycle + 1)
            if cycle + 2 in intr:
                S += X * (cycle + 2) 
            X += int(ins[1])
            cycle += 2
        else:
            if cycle + 1 in intr:
                S += X * (cycle + 1)
            cycle += 1
    return S
        


def part2(inp):
    board = []
    for _ in range(6):
        board.append([' ' for _ in range(40)])
    X = 1
    CR = 0
    CC = 0
    for ins in inp:
        if len(ins) == 2:
            if abs(CC - X) < 2:
                board[CR][CC] = '#'
            CC += 1
            if CC == 40:
                CR += 1
                CC = 0
            if abs(CC - X) < 2:
                board[CR][CC] = '#'
            CC += 1
            if CC == 40:
                CR += 1
                CC = 0
            X += int(ins[1])
        else:
            if abs(CC - X) < 2:
                board[CR][CC] = '#'
            CC += 1
            if CC == 40:
                CR += 1
                CC = 0
    for row in board:
        print(''.join(row))


if __name__ == '__main__':
    main()

