import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def neighbours(inp, x, y):
    n = []
    if x > 0:
        n += [inp[y][x-1]]
    if y > 0:
        n += [inp[y-1][x]]
    if x < len(inp[y]) - 1:
        n += [inp[y][x+1]]
    if y < len(inp) - 1:
        n += [inp[y+1][x]]

    return n


def part1(inp):
    result = 0
    for y, row in enumerate(inp):
        for x, col in enumerate(row):
            if min(neighbours(inp, x, y)) > col:
                result += col + 1

    return result


def neighbours_xy(inp, x, y):
    n = []
    if x > 0:
        n += [(x - 1, y)]
    if y > 0:
        n += [(x, y - 1)]
    if x < len(inp[y]) - 1:
        n += [(x + 1, y)]
    if y < len(inp) - 1:
        n += [(x, y + 1)]

    return n


def get_sink_size(inp, x, y):
    elts = [(x, y)]
    size = 1
    inp[y][x] = -1

    while elts:
        x_, y_ = elts.pop(0)

        for xn, yn in neighbours_xy(inp, x_, y_):
            if inp[yn][xn] >= 0 and inp[yn][xn] < 9:
                inp[yn][xn] = -1
                size += 1
                elts.append((xn, yn))
            
    return size


def part2(inp):
    sinks = []
    for y, row in enumerate(inp):
        for x, col in enumerate(row):
            if min(neighbours(inp, x, y)) > col:
                sinks.append((x, y))

    sizes = []
    for sink in sinks:
        sizes.append(get_sink_size(inp, *sink))
    sizes.sort()

    return sizes[-3] * sizes[-2] * sizes[-1]


def main():
    with open(f'{dir_path}/../../inputs/day09/input') as f:
        inp = list(map(lambda x: list(map(int, list(x))), f.read().strip().split('\n')))
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

