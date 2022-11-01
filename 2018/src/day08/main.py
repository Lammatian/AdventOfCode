import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day08/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day08/input'

    with open(filepath) as f:
        inp = list(map(lambda x: list(map(int, x.split())), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    def parse_val(curr, inp):
        vals = inp[curr]
        mdata_len = inp[curr + 1]
        mdata_sum = 0
        if vals == 0:
            return (curr + 2 + mdata_len, sum(inp[curr + 2:curr + 2 + mdata_len]))

        curr += 2
        for _ in range(vals):
            curr, mdata_sum_ = parse_val(curr, inp)
            mdata_sum += mdata_sum_

        mdata_sum += sum(inp[curr:curr + mdata_len])

        return (curr + mdata_len, mdata_sum)
        
    return parse_val(0, inp)[1]


def part2(inp):
    def parse_val(curr, inp):
        print(curr)
        print(inp[curr:])
        vals = inp[curr]
        mdata_len = inp[curr + 1]
        mdata_sum = 0
        if vals == 0:
            return (curr + 2 + mdata_len, sum(inp[curr + 2:curr + 2 + mdata_len]))

        curr += 2
        child_values = {}
        for i in range(vals):
            curr, mdata_sum_ = parse_val(curr, inp)
            child_values[i + 1] = mdata_sum_

        mdata_sum += sum(child_values[i] for i in inp[curr:curr + mdata_len] if i in child_values)

        return (curr + mdata_len, mdata_sum)
        
    return parse_val(0, inp)[1]


if __name__ == '__main__':
    main()

