import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    curr = 0
    result = 0
    while curr < len(inp):
        result += 1
        tmp = inp[curr]
        inp[curr] += 1
        curr += tmp
    return result


def part2(inp):
    curr = 0
    result = 0
    while curr < len(inp):
        result += 1
        tmp = inp[curr]
        if inp[curr] < 3:
            inp[curr] += 1
        else:
            inp[curr] -= 1
        curr += tmp
    return result


def main():
    with open(f'{dir_path}/../../inputs/day05/input') as f:
        inp = list(map(lambda x: int(x), f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

