import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def triangle(sides):
    s = sorted(sides)
    return s[0] + s[1] > s[2]


def part1(inp):
    return sum([triangle(sides) for sides in inp])


def part2(inp):
    count = 0
    for r in range(0, len(inp), 3):
        for c in range(3):
            sides = [inp[r][c], inp[r + 1][c], inp[r + 2][c]]
            if triangle(sides):
                count += 1

    return count


def main():
    with open(f'{dir_path}/../../inputs/day03/input') as f:
        inp = list(map(lambda x: list(map(int, x.strip().split())), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

