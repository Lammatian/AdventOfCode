import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import numpy as np


def iter(elts, lengths, curr, skip):
    for i in lengths:
        elts = np.roll(elts, -curr)
        elts = np.concatenate([np.flip(elts[:i]), elts[i:]])
        elts = np.roll(elts, curr)
        curr = (curr + skip + i) % len(elts)
        skip += 1
    return elts, lengths, curr, skip


def part1(inp):
    elts, _, _, _ = iter(np.arange(256), inp, 0, 0)
    return elts[0] * elts[1]
        

def part2(inp):
    lengths = []
    for c in inp:
        lengths.append(ord(c))
    lengths += [17, 31, 73, 47, 23]
    elts = np.arange(256)
    curr = 0
    skip = 0
    for _ in range(64):
        elts, lengths, curr, skip = iter(elts, lengths, curr, skip)
    result = []
    for i in range(16):
        result.append(np.bitwise_xor.reduce(elts[i * 16:(i + 1) * 16]))
    return ''.join(hex(r)[2:] for r in result)


def main():
    with open(f'{dir_path}/../../inputs/day10/input') as f:
        inp = list(map(lambda x: list(map(int, x.split(','))), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))

    with open(f'{dir_path}/../../inputs/day10/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

