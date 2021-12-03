import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    counts = {i: 0 for i in range(len(inp[0]))}
    for bin in inp:
        for i, v in enumerate(bin):
            if v == '1':
                counts[i] += 1

    res = int(''.join(['1' if counts[i] > len(inp)//2 else '0' for i in range(len(inp[0]))]), 2)

    return res * ((1 << len(inp[0])) - res - 1)


def part2(inp):
    inp_ = inp[:]
    for i in range(len(inp_[0])):
        count = sum([bin[i] == '1' for bin in inp_])
        if count >= len(inp_)/2:
            inp_ = [bin for bin in inp_ if bin[i] == '1']
        else:
            inp_ = [bin for bin in inp_ if bin[i] == '0']

        if len(inp_) == 1:
            res1 = int(inp_[0], 2)

    inp_ = inp[:]
    for i in range(len(inp_[0])):
        count = sum([bin[i] == '1' for bin in inp_])
        if count < len(inp_)/2:
            inp_ = [bin for bin in inp_ if bin[i] == '1']
        else:
            inp_ = [bin for bin in inp_ if bin[i] == '0']

        if len(inp_) == 1:
            res2 = int(inp_[0], 2)

    return res1 * res2


def main():
    with open(f'{dir_path}/../../inputs/day03/input') as f:
        inp = f.read().strip().split('\n')
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

