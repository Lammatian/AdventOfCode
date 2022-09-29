import os
from collections import defaultdict
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    regs = defaultdict(int)
    for ins in inp:
        if ins[-2] == '==':
            if regs[ins[-3]] == int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '>=':
            if regs[ins[-3]] >= int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '<=':
            if regs[ins[-3]] <= int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '>':
            if regs[ins[-3]] > int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '<':
            if regs[ins[-3]] < int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '!=':
            if regs[ins[-3]] != int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
    return max(regs.values())



def part2(inp):
    regs = defaultdict(int)
    result = 0
    for ins in inp:
        if ins[-2] == '==':
            if regs[ins[-3]] == int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '>=':
            if regs[ins[-3]] >= int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '<=':
            if regs[ins[-3]] <= int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '>':
            if regs[ins[-3]] > int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '<':
            if regs[ins[-3]] < int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        elif ins[-2] == '!=':
            if regs[ins[-3]] != int(ins[-1]):
                if ins[1] == 'inc':
                    regs[ins[0]] += int(ins[2])
                else:
                    regs[ins[0]] -= int(ins[2])
        result = max(result, max(regs.values()))
    return result


def main():
    with open(f'{dir_path}/../../inputs/day08/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

