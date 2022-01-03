import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import Counter


def part1(inp):
    result = ''
    for col in range(len(inp[0])):
        c = Counter([line[col] for line in inp])
        result += c.most_common()[0][0]

    return result


def part2(inp):
    result = ''
    for col in range(len(inp[0])):
        c = Counter([line[col] for line in inp])
        result += c.most_common()[-1][0]

    return result


def main():
    with open(f'{dir_path}/../../inputs/day06/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

