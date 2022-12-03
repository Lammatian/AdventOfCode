import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np
from pyutils import *


def parse(line):
    return line


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
    result = 0
    for i in inp:
        a, b = i[:len(i)//2], i[len(i)//2:]
        s = set(a).intersection(b)
        for c in s:
            if c.isupper():
                result += ord(c) - ord('A') + 27
            else:
                result += ord(c) - ord('a') + 1
    return result


def part2(inp):
    result = 0
    for i in range(0, len(inp), 3):
        sub = inp[i:i+3]
        s = set(sub[0]).intersection(set(sub[1])).intersection(set(sub[2]))
        for c in s:
            if c.isupper():
                result += ord(c) - ord('A') + 27
            else:
                result += ord(c) - ord('a') + 1
    return result
    

if __name__ == '__main__':
    main()

