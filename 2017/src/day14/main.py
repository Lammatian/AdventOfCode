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


def khash(key):
    key = key + [17, 31, 73, 47, 23]
    elts = np.arange(256)
    curr = 0
    skip = 0
    for _ in range(64):
        elts, key, curr, skip = iter(elts, key, curr, skip)
    result = []
    for i in range(16):
        result.append(np.bitwise_xor.reduce(elts[i * 16:(i + 1) * 16]))
    return ''.join(bin(r)[2:].zfill(8) for r in result)


def part1(inp):
    result = 0
    inp += [ord('-')]
    for i in range(128):
        kh = khash(inp + [ord(c) for c in str(i)])
        result += kh.count('1')
    return result


def part2(inp):
    board = []
    inp += [ord('-')]
    for i in range(128):
        kh = khash(inp + [ord(c) for c in str(i)])
        board.append(list(int(c) for c in kh))

    groups = 0
    C = len(board)
    R = len(board[0])
    for c in range(C):
        for r in range(R):
            if board[c][r] < 1:
                continue
            q = [(c, r)]
            groups += 1
            while q:
                c_, r_ = q.pop(0)
                DC = [1, 0, -1, 0]
                DR = [0, 1, 0, -1]
                for dc, dr in zip(DC, DR):
                    cc = c_ + dc
                    rr = r_ + dr
                    if 0 <= cc < C and 0 <= rr < R and board[cc][rr] > 0:
                        board[cc][rr] = -1
                        q.append((cc, rr))
    return groups


def main():
    with open(f'{dir_path}/../../inputs/day14/input') as f:
        inp = list(map(lambda x: list(map(ord, x)), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

