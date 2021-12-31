import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from functools import lru_cache
from collections import Counter

def sum_of_divisors(num):
    s = 0
    i = 1
    while i * i <= num:
        if num % i == 0:
            s += i
            if i != num // i:
                s += num // i
        i += 1
    return 10 * s


def part1(inp):
    i = 2
    while True:
        if sum_of_divisors(i) >= inp:
            return i
        i += 1


def weird_divisors(num):
    s = 0
    for i in range(1, 50):
        if num % i == 0:
            s += num // i
    return 11 * s


def part2(inp):
    i = 1
    while True:
        if weird_divisors(i) >= inp:
            return i
        i += 1


def main():
    with open(f'{dir_path}/../../inputs/day20/input') as f:
        inp = int(f.read().strip())

    print(inp)
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

