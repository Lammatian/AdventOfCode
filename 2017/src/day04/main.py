import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    return sum(1 for row in inp if len(set(row)) == len(row))


def part2(inp):
    for i, row in enumerate(inp):
        inp[i] = [''.join(sorted(r)) for r in row]
    return sum(1 for row in inp if len(set(row)) == len(row))


def main():
    with open(f'{dir_path}/../../inputs/day04/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

