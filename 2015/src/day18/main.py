import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def draw(lights):
    for line in lights:
        print(''.join(line))
    print()


def neighbours(x, y, w, h):
    ns = []
    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            if dx != 0 or dy != 0:
                if 0 <= x + dx < w and 0 <= y + dy < h:
                    ns.append((x + dx, y + dy))

    return ns


def perform_step(lights):
    W = len(lights[0])
    H = len(lights)
    new_on = set()
    for y, row in enumerate(lights):
        for x, c in enumerate(row):
            ns = neighbours(x, y, W, H)
            ns_on = 0
            for nx, ny in ns:
                if lights[ny][nx] == '#':
                    ns_on += 1

            if c == '#' and 2 <= ns_on <= 3:
                new_on.add((x, y))
            if c == '.' and ns_on == 3:
                new_on.add((x, y))

    new_lights = [['.' for _ in range(W)] for _ in range(H)]

    for x, y in new_on:
        new_lights[y][x] = '#'

    return new_lights


def part1(inp):
    for step in range(100):
        inp = perform_step(inp)

    result = 0
    for line in inp:
        result += sum(x == '#' for x in line)

    return result


def perform_step2(lights):
    W = len(lights[0])
    H = len(lights)
    new_on = set()
    for y, row in enumerate(lights):
        for x, c in enumerate(row):
            ns = neighbours(x, y, W, H)
            ns_on = 0
            for nx, ny in ns:
                if lights[ny][nx] == '#':
                    ns_on += 1

            if c == '#' and 2 <= ns_on <= 3:
                new_on.add((x, y))
            if c == '.' and ns_on == 3:
                new_on.add((x, y))

    new_lights = [['.' for _ in range(W)] for _ in range(H)]
    for x in [0, -1]:
        for y in [0, -1]:
            new_lights[y][x] = '#'

    for x, y in new_on:
        new_lights[y][x] = '#'

    return new_lights


def part2(inp):
    for x in [0, -1]:
        for y in [0, -1]:
            inp[y][x] = '#'
    
    for step in range(100):
        inp = perform_step2(inp)

    result = 0
    for line in inp:
        result += sum(x == '#' for x in line)

    return result

def main():
    with open(f'{dir_path}/../../inputs/day18/input') as f:
        inp = list(map(lambda x: list(x), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

