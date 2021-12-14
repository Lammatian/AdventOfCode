import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import Counter


def part1(start, rules):
    for _ in range(10):
        new_start = [start[0]]
        for a, b in zip(start, start[1:]):
            new_start += [rules[a + b], b]

        start = new_start

    c = Counter(start)
    return max(c.values()) - min(c.values())


def part2(start, rules):
    first = start[0]
    last = start[-1]
    pairs = Counter(zip(start, start[1:]))
    for _ in range(40):
        new_pairs = Counter()
        
        for (a, b), val in pairs.items():
            c = rules[a + b]
            new_pairs[(a, c)] += val
            new_pairs[(c, b)] += val

        pairs = new_pairs

    c = Counter()
    for (a, b), v in pairs.items():
        c[a] += v
        c[b] += v

    c[first] += 1
    c[last] += 1

    return (max(c.values()) - min(c.values())) // 2


def main():
    with open(f'{dir_path}/../../inputs/day14/sample') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    start = list(inp[0])
    rules = list(map(lambda x: x.split(' -> '), inp[2:]))
    print(inp)
    print(start)
    print(dict(rules))
    
    print(part1(start[:], dict(rules)))
    print(part2(start[:], dict(rules)))


if __name__ == '__main__':
    main()

