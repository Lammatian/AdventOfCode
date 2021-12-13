import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(ins, folds):
    visible = set(ins)
    fold = folds[0]

    to_rem = set()
    to_add = set()
    if fold[0] == 'x':
        for x, y in visible:
            if x > fold[1]:
                to_rem.add((x, y))
                to_add.add((2*fold[1] - x, y))
    if fold[0] == 'y':
        for x, y in visible:
            if y > fold[1]:
                to_rem.add((x, y))
                to_add.add((x, 2*fold[1] - y))

    for p in to_rem:
        visible.remove(p)

    for p in to_add:
        visible.add(p)

    return len(visible)


def part2(ins, folds):
    visible = set(ins)

    for fold in folds:
        to_rem = set()
        to_add = set()
        if fold[0] == 'x':
            for x, y in visible:
                if x > fold[1]:
                    to_rem.add((x, y))
                    to_add.add((2*fold[1] - x, y))
        if fold[0] == 'y':
            for x, y in visible:
                if y > fold[1]:
                    to_rem.add((x, y))
                    to_add.add((x, 2*fold[1] - y))

        for p in to_rem:
            visible.remove(p)

        for p in to_add:
            visible.add(p)

    max_x = max([x for x, y in visible])
    max_y = max([y for x, y in visible])
    board = [['.' for _ in range(max_x + 1)] for _ in range(max_y + 1)]

    for x, y in visible:
        board[y][x] = '#'

    print('\n'.join(list(map(lambda x: ''.join(x), board))))

    return len(visible)


def main():
    with open(f'{dir_path}/../../inputs/day13/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    ins = [tuple(map(int, x.split(','))) for x in inp if x and x[0].isdigit()]
    folds = [(x.split('=')[0][-1], int(x.split('=')[1])) for x in inp if x and x[0] == 'f']
    print(ins, folds)
    
    print(part1(ins[:], folds[:]))
    print(part2(ins[:], folds[:]))


if __name__ == '__main__':
    main()

