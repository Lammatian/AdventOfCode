import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def right(inp, x, y):
    return ((x + 1) % len(inp[0]), y)


def down(inp, x, y):
    return (x, (y + 1) % len(inp))


def part1(inp):
    righties = set()
    downies = set()
    for y, line in enumerate(inp):
        for x, c in enumerate(line):
            if c == '>':
                righties.add((x, y))
            elif c == 'v':
                downies.add((x, y))

    turn = 0
    changed = True
    while changed:
        changed = False
        new_r = set()
        for x, y in righties:
            if right(inp, x, y) not in righties and right(inp, x, y) not in downies:
                new_r.add(right(inp, x, y))
                changed = True
            else:
                new_r.add((x, y))

        righties = new_r

        new_d = set()
        for x, y in downies:
            if down(inp, x, y) not in righties and down(inp, x, y) not in downies:
                new_d.add(down(inp, x, y))
                changed = True
            else:
                new_d.add((x, y))

        downies = new_d
        turn += 1

    return turn


def part2(inp):
    print('Merry Christmas')


def main():
    with open(f'{dir_path}/../../inputs/day25/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

