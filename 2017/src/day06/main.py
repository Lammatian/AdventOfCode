import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp: list):
    seen = set()
    max_val = max(inp)
    max_idx = inp.index(max_val)
    inp[max_idx] = 0
    for i in range(len(inp)):
        inp[i] += max_val // len(inp)
    for i in range(max_val % len(inp)):
        inp[(max_idx + 1 + i) % len(inp)] += 1
    count = 1
    while tuple(inp) not in seen:
        seen.add(tuple(inp))
        max_val = max(inp)
        max_idx = inp.index(max_val)
        inp[max_idx] = 0
        for i in range(len(inp)):
            inp[i] += max_val // len(inp)
        for i in range(max_val % len(inp)):
            inp[(max_idx + 1 + i) % len(inp)] += 1
        count += 1

    return count


def part2(inp):
    seen = {}
    count = 0
    while tuple(inp) not in seen:
        seen[tuple(inp)] = count
        max_val = max(inp)
        max_idx = inp.index(max_val)
        inp[max_idx] = 0
        for i in range(len(inp)):
            inp[i] += max_val // len(inp)
        for i in range(max_val % len(inp)):
            inp[(max_idx + 1 + i) % len(inp)] += 1
        count += 1

    return count - seen[tuple(inp)]


def main():
    with open(f'{dir_path}/../../inputs/day06/input') as f:
        inp = list(map(lambda x: list(map(int, x.split())), f.read().strip().split('\n')))[0]

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

