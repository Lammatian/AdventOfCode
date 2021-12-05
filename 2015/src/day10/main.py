import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def iterate(inp):
    last = ''
    count = 0
    result = ''
    for c in inp:
        if c == last:
            count += 1
        else:
            result += (str(count) if count > 0 else '') + last
            count = 1
            last = c

    result += str(count) + last

    return result


def part1(inp):
    for _ in range(40):
        inp = iterate(inp) 

    return len(inp)


def part2(inp):
    for _ in range(50):
        inp = iterate(inp) 

    return len(str(inp))


def main():
    with open(f'{dir_path}/../../inputs/day10/input') as f:
        inp = f.read().strip()

    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

