import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import Counter


def part1(inp):
    result = 0
    for name, sid, csum in inp:
        c = Counter(name.replace('-', ''))
        mcl = ''
        cmcl, lcount = c.most_common()[0]
        for letter, count in c.most_common()[1:]:
            if count != lcount:
                mcl += ''.join(sorted(cmcl))
                cmcl = letter
            else:
                cmcl += letter
            lcount = count
        mcl += ''.join(sorted(cmcl))
        mcl = mcl[:5]

        if mcl == csum:
            result += sid

    return result


def decode_letter(c, rot):
    alph = 'abcdefghijklmnopqrstuvwxyz'
    oc = ord(c) - ord('a')
    return alph[(oc + rot) % len(alph)]


def part2(inp):
    for name, sid, csum in inp:
        real_name = ''
        for letter in name:
            if letter == '-':
                real_name += ' '
            else:
                real_name += decode_letter(letter, sid)
        if real_name[:5] == 'north':
            return sid


def main():
    with open(f'{dir_path}/../../inputs/day04/input') as f:
        inp = list(map(lambda x: (x[:-11], int(x[-10:-7]), x[-6:-1]), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

