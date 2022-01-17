import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def isnum(s):
    return s[0] == '-' or s.isnumeric()


def simulate(inp, val_a):
    regs = defaultdict(int)
    regs['a'] = val_a
    pc = 0
    isjump = False

    while pc < len(inp):
        ins = inp[pc]
        cmd = ins[0]
        if cmd == 'inc':
            reg = ins[1]
            regs[reg] += 1
        elif cmd == 'dec':
            reg = ins[1]
            regs[reg] -= 1
        elif cmd == 'jnz':
            val1 = int(ins[1]) if isnum(ins[1]) else regs[ins[1]]
            val2 = int(ins[2]) if isnum(ins[2]) else regs[ins[2]]
            if val1 != 0:
                pc += val2
                isjump = True
        elif cmd == 'cpy':
            val1 = int(ins[1]) if isnum(ins[1]) else regs[ins[1]]
            reg2 = ins[2]
            regs[reg2] = val1
        elif cmd == 'out':
            print(regs[ins[1]])

        if not isjump:
            pc += 1
        isjump = False

    return regs['a']


def part1(inp):
    minimum = int(inp[1][1]) * int(inp[2][1])

    x = 1
    while x < minimum:
        x = 2 * x + (1 - x % 2)

    return x - minimum


def part2(inp):
    return ":)"


def main():
    with open(f'{dir_path}/../../inputs/day25/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

