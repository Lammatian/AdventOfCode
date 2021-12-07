import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    min_ = min(inp)
    max_ = max(inp)
    result = 0
    best_result = 9999999999999

    for i in range(min_, max_ + 1):
        for val in inp:
            result += abs(i - val)

        best_result = min(result, best_result)
        result = 0

    return best_result


def part2(inp):
    min_ = min(inp)
    max_ = max(inp)
    result = 0
    best_result = 9999999999999

    for i in range(min_, max_ + 1):
        for val in inp:
            cost = ((abs(val - i) + 1) * abs(val - i)) // 2
            result += abs(cost)

        best_result = min(result, best_result)
        result = 0

    return best_result


def main():
    with open(f'{dir_path}/../../inputs/day07/input') as f:
        inp = list(map(int, f.read().strip().split(',')))

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

