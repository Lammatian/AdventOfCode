import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def part1(inp):
    regs = defaultdict(int)
    pc = 0
    while pc < len(inp):
        ins = inp[pc]
        cmd = ins[0]
        r1 = ins[1]
        jumped = False
        if cmd == 'hlf':
            regs[r1] //= 2
        elif cmd == 'tpl':
            regs[r1] *= 3
        elif cmd == 'inc':
            regs[r1] += 1
        elif cmd == 'jmp':
            pc += int(r1)
            jumped = True
        elif cmd == 'jio':
            r1 = r1[:-1]
            if regs[r1] == 1:
                pc += int(ins[2])
                jumped = True
        elif cmd == 'jie':
            r1 = r1[:-1]
            if regs[r1] % 2 == 0:
                pc += int(ins[2])
                jumped = True

        if not jumped:
            pc += 1

    return regs['b']


def part2(inp):
    regs = defaultdict(int)
    regs['a'] = 1
    pc = 0
    while pc < len(inp):
        ins = inp[pc]
        cmd = ins[0]
        r1 = ins[1]
        jumped = False
        if cmd == 'hlf':
            regs[r1] //= 2
        elif cmd == 'tpl':
            regs[r1] *= 3
        elif cmd == 'inc':
            regs[r1] += 1
        elif cmd == 'jmp':
            pc += int(r1)
            jumped = True
        elif cmd == 'jio':
            r1 = r1[:-1]
            if regs[r1] == 1:
                pc += int(ins[2])
                jumped = True
        elif cmd == 'jie':
            r1 = r1[:-1]
            if regs[r1] % 2 == 0:
                pc += int(ins[2])
                jumped = True

        if not jumped:
            pc += 1

    return regs['b']


def main():
    with open(f'{dir_path}/../../inputs/day23/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

