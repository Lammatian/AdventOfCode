import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict
from copy import deepcopy


def isnum(s):
    return s[0] == '-' or s.isnumeric()


def fact(n):
    f = 1
    while n > 0:
        f, n = f * n, n - 1
    return f


def part1(inp):
    regs = defaultdict(int)
    regs['a'] = 7
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
        elif cmd == 'tgl':
            val = int(ins[1]) if isnum(ins[1]) else regs[ins[1]]
            idx = pc + val
            if idx < 0 or idx >= len(inp):
                pc += 1
                continue
            if len(inp[idx]) == 2:
                if inp[idx][0] == 'inc':
                    inp[idx][0] = 'dec'
                else:
                    inp[idx][0] = 'inc'
            else:
                if inp[idx][0] == 'jnz':
                    inp[idx][0] = 'cpy'
                else:
                    inp[idx][0] = 'jnz'

        if not isjump:
            pc += 1
        isjump = False

    return regs['a']


def part1_(inp):
    return fact(7) + int(inp[-7][1]) * int(inp[-6][1])


def part2(inp):
    return fact(12) + int(inp[-7][1]) * int(inp[-6][1])


def main():
    with open(f'{dir_path}/../../inputs/day23/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part1_(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

