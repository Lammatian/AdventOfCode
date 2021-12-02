import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    x, y = 0, 0

    for i, v in inp:
        if i == "forward":
            x += v
        elif i == "down":
            y -= v
        else:
            y += v

    return abs(x * y)

def part2(inp):
    x, d, a = 0, 0, 0

    for i, v in inp:
        if i == "forward":
            x += v
            d += a * v
        elif i == "down":
            a -= v
        else:
            a += v

    return abs(x * d)


def main():
    with open(f'{dir_path}/../../inputs/day02/input') as f:
        inp = list(map(lambda x: (x.split()[0], int(x.split()[1])), f.read().strip().split('\n')))
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

