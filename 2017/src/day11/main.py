import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    dirs = {
        'nw': 0,
        'ne': 0,
        'n': 0
    }
    for d in inp:
        if d[0] == 'n':
            dirs[d] += 1
        elif d == 's':
            dirs['n'] -= 1
        elif d == 'se':
            dirs['nw'] -= 1
        elif d == 'sw':
            dirs['ne'] -= 1

    return get_dist(dirs)


def get_dist(dirs: dict):
    return sum(map(abs, dirs.values())) - min(map(abs, dirs.values()))


def part2(inp):
    dirs = {
        'nw': 0,
        'ne': 0,
        'n': 0
    }
    max_dist = 0
    for d in inp:
        if d[0] == 'n':
            dirs[d] += 1
        elif d == 's':
            dirs['n'] -= 1
        elif d == 'se':
            dirs['nw'] -= 1
        elif d == 'sw':
            dirs['ne'] -= 1
        max_dist = max(max_dist, get_dist(dirs))
    return max_dist


def main():
    with open(f'{dir_path}/../../inputs/day11/input') as f:
        inp = list(map(lambda x: x.split(','), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

