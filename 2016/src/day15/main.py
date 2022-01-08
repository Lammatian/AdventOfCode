import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    discs = []
    for d in inp:
        ds = d.split()
        discs.append((int(ds[3]), int(ds[-1][:-1])))

    time = 0
    while True:
        discs = [(ps, (p + 1) % ps) for ps, p in discs]

        can_press = True
        for i in range(len(discs)):
            if discs[i][0] - discs[i][1] != i + 1:
                can_press = False
                break

        if can_press:
            return time + 1

        time += 1


def part2(inp):
    discs = []
    for d in inp:
        ds = d.split()
        discs.append((int(ds[3]), int(ds[-1][:-1])))

    discs.append((11, 0))
    time = 0
    while True:
        discs = [(ps, (p + 1) % ps) for ps, p in discs]

        can_press = True
        for i in range(len(discs)):
            if discs[i][0] - discs[i][1] != i + 1:
                can_press = False
                break

        if can_press:
            return time + 1

        time += 1


def main():
    with open(f'{dir_path}/../../inputs/day15/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

