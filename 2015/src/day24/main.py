import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import combinations


def part1(inp):
    S = sum(inp)
    best_qe = 1e15
    # S // 3 is even, so we need at least 6 elements
    for combo in combinations(inp, 6):
        if sum(combo) == S // 3:
            qe = 1
            for c in combo:
                qe *= c
            best_qe = min(qe, best_qe)

    return best_qe


def part2(inp):
    S = sum(inp)
    best_qe = 1e15
    # S // 4 is odd, so we need at least 5 elements
    for combo in combinations(inp, 5):
        if sum(combo) == S // 4:
            qe = 1
            for c in combo:
                qe *= c
            best_qe = min(qe, best_qe)

    return best_qe


def main():
    with open(f'{dir_path}/../../inputs/day24/input') as f:
        inp = list(map(lambda x: int(x), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

