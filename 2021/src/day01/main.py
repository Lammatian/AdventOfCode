import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    last = 999
    count = 0
    for i in inp:
        if last < i:
            count += 1
        last = i

    return count


def part2(inp):
    last = 999
    count = 0
    for x in zip(inp, inp[1:], inp[2:]):
        if last < sum(x):
            count += 1
        last = sum(x) 

    return count


def main():
    with open(f'{dir_path}/../../inputs/day01/input') as f:
        inp = list(map(int, f.read().strip().split('\n')))
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

