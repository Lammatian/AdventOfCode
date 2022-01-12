import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    inp.sort()
    ch = 0
    for a, b in inp:
        if a > ch + 1:
            return ch + 1
        ch = max(ch, b)


def part2(inp):
    inp.sort()
    ch = 0
    ranges = [[0, 0]]
    for a, b in inp:
        cl, ch = ranges[-1]
        if a <= ch + 1:
            ranges[-1] = [min(a, cl), max(b, ch)]
        else:
            ranges.append([a, b])

    result = 0
    for (a, b), (c, d) in zip(ranges, ranges[1:]):
        result += c - b - 1

    result += 2**32 - 1 - ranges[-1][1]
    return result




def main():
    with open(f'{dir_path}/../../inputs/day20/input') as f:
        inp = list(map(lambda x: list(map(int, x.split('-'))), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

