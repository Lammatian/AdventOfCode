import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import permutations


def part1(s, inp):
    for ins in inp:
        if ins[0] == 'rotate':
            if ins[1] == 'based':
                pos = s.index(ins[-1])
                rots = (1 + pos + (1 if pos >= 4 else 0)) % len(s)
                s = s[-rots:] + s[:-rots]
            elif ins[1] == 'left':
                rots = int(ins[2])
                s = s[rots:] + s[:rots]
            elif ins[1] == 'right':
                rots = int(ins[2])
                s = s[-rots:] + s[:-rots]
        elif ins[0] == 'swap':
            if ins[1] == 'position':
                p1, p2 = sorted([int(ins[2]), int(ins[-1])])
                s[p1], s[p2] = s[p2], s[p1]
            elif ins[1] == 'letter':
                p1, p2 = sorted([s.index(ins[2]), s.index(ins[-1])])
                s[p1], s[p2] = s[p2], s[p1]
        elif ins[0] == 'reverse':
            f, t = sorted([int(ins[2]), int(ins[-1])])
            s = s[:f] + s[f:t+1][::-1] + s[t+1:]
        elif ins[0] == 'move':
            f, t = int(ins[2]), int(ins[-1])
            tmp = s[f]
            s = s[:f] + s[f + 1:]
            s = s[:t] + [tmp] + s[t:]

    return ''.join(s)


def part2(s_, inp):
    for perm in permutations(list('abcdefgh')):
        s = list(perm)
        for ins in inp:
            if ins[0] == 'rotate':
                if ins[1] == 'based':
                    pos = s.index(ins[-1])
                    rots = (1 + pos + (1 if pos >= 4 else 0)) % len(s)
                    s = s[-rots:] + s[:-rots]
                elif ins[1] == 'left':
                    rots = int(ins[2])
                    s = s[rots:] + s[:rots]
                elif ins[1] == 'right':
                    rots = int(ins[2])
                    s = s[-rots:] + s[:-rots]
            elif ins[0] == 'swap':
                if ins[1] == 'position':
                    p1, p2 = sorted([int(ins[2]), int(ins[-1])])
                    s[p1], s[p2] = s[p2], s[p1]
                elif ins[1] == 'letter':
                    p1, p2 = sorted([s.index(ins[2]), s.index(ins[-1])])
                    s[p1], s[p2] = s[p2], s[p1]
            elif ins[0] == 'reverse':
                f, t = sorted([int(ins[2]), int(ins[-1])])
                s = s[:f] + s[f:t+1][::-1] + s[t+1:]
            elif ins[0] == 'move':
                f, t = int(ins[2]), int(ins[-1])
                tmp = s[f]
                s = s[:f] + s[f + 1:]
                s = s[:t] + [tmp] + s[t:]

        if s == s_:
            return ''.join(perm)


def main():
    with open(f'{dir_path}/../../inputs/day21/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(list('abcdefgh'), inp[:]))
    print(part2(list('fbgdceah'), inp[:]))


if __name__ == '__main__':
    main()

