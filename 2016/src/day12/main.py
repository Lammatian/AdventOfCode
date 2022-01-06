import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def fib(n):
    a, b = 0, 1
    while n > 0:
        a, b = b, a + b
        n -= 1

    return b


def part1(inp):
    n = int(inp[2][1]) + 1
    c = int(inp[-7][1])
    d = int(inp[-6][1])
    return fib(n) + c * d


def part2(inp):
    n = int(inp[2][1]) + int(inp[5][1]) + 1
    c = int(inp[-7][1])
    d = int(inp[-6][1])
    return fib(n) + c * d


def main():
    with open(f'{dir_path}/../../inputs/day12/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

