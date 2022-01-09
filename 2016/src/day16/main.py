import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp, disk_size):
    while len(inp) < disk_size:
        tmp = inp[::-1]
        tmp = ''.join(['1' if c == '0' else '0' for c in tmp])
        inp = inp + '0' + tmp

    inp = inp[:disk_size]

    while len(inp) % 2 == 0:
        new_inp = ''
        for i in range(0, len(inp), 2):
            i1, i2 = inp[i], inp[i + 1]
            if i1 == i2:
                new_inp += '1'
            else:
                new_inp += '0'

        inp = new_inp

    return inp


def part2(inp, disk_size):
    while len(inp) < disk_size:
        tmp = inp[::-1]
        tmp = ''.join(['1' if c == '0' else '0' for c in tmp])
        inp = inp + '0' + tmp

    inp = inp[:disk_size]

    while len(inp) % 2 == 0:
        new_inp = ''
        for i in range(0, len(inp), 2):
            i1, i2 = inp[i], inp[i + 1]
            if i1 == i2:
                new_inp += '1'
            else:
                new_inp += '0'

        inp = new_inp

    return inp


def main():
    with open(f'{dir_path}/../../inputs/day16/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:], 272))
    print(part2(inp[:], 35651584))


if __name__ == '__main__':
    main()

