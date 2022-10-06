import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    result = 0
    for k, v in inp.items():
        if k % ((v - 1) * 2) == 0:
            result += k * v
    return result


def part2(inp):
    i = 0
    def go_through(inp, pos):
        for k, v in inp.items():
            if (k + pos[k]) % ((v - 1) * 2) == 0:
                return False
        return True
        
    pos = {k: k % ((v - 1) * 2) for k, v in inp.items()}
    i = 0
    while not go_through(inp, pos):
        i += 1
        for k, _ in pos.items():
            pos[k] = i % (2 * (inp[k] - 1))
    return i


def main():
    with open(f'{dir_path}/../../inputs/day13/input') as f:
        inp = dict(list(map(lambda x: list(map(int, x.split(': '))), f.read().strip().split('\n'))))

    print(inp)

    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

