import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    last = inp[-1]
    result = 0
    for c in inp:
        if c == last:
            result += int(c)
        last = c

    return result


def part2(inp):
    result = 0
    for i, c in enumerate(inp):
        if c == inp[(len(inp) // 2 + i) % len(inp)]:
            result += int(c)

    return result


def main():
    with open(f'{dir_path}/../../inputs/day01/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:][0]))
    print(part2(inp[:][0]))


if __name__ == '__main__':
    main()

