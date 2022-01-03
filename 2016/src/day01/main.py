import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    didx = 0
    loc = [0, 0]

    for ins in inp:
        if ins[0] == 'L':
            didx = (didx + 3) % 4
        else:
            didx = (didx + 1) % 4

        m = int(ins[1:])
        dx, dy = dirs[didx]
        loc[0] += m * dx
        loc[1] += m * dy

    return sum(map(abs, loc))


def part2(inp):
    dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    didx = 0
    loc = [0, 0]
    seen = set()

    for ins in inp:
        if ins[0] == 'L':
            didx = (didx + 3) % 4
        else:
            didx = (didx + 1) % 4

        m = int(ins[1:])
        dx, dy = dirs[didx]
        for i in range(1, m + 1):
            new_loc = (loc[0] + i * dx, loc[1] + i * dy)
            if new_loc in seen:
                return sum(map(abs, new_loc))
            seen.add(new_loc)

        loc[0] += m * dx
        loc[1] += m * dy

    return sum(map(abs, loc))


def main():
    with open(f'{dir_path}/../../inputs/day01/input') as f:
        inp = list(map(lambda x: x.split(', '), f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

