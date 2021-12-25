import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict
from itertools import product


def execute(inp, inputs):
    mem = defaultdict(int)
    z = mem['z']

    for ins in inp:
        if ins[0] == 'inp':
            mem[ins[1]] = inputs.pop(0)
            continue

        command, x, y = ins
        a = x
        b = int(y) if y.isnumeric() or y[0] == '-' else mem[y]
        if command == 'add':
            mem[a] = mem[a] + b
        elif command == 'mul':
            mem[a] = mem[a] * b
        elif command == 'div':
            mem[a] = mem[a] // b
        elif command == 'mod':
            mem[a] = mem[a] % b
        elif command == 'eql':
            mem[a] = 1 if mem[a] == b else 0
        if z != mem['z']:
            z = mem['z']

    return mem['z']


def find_model(X, W, Y, max_monad=True):
    stack = []
    result = [-1] * 14
    for i, w in enumerate(W):
        if w == 1:
            stack.append(i)
        else:
            j = stack.pop()
            diff = X[i] + Y[j]
            if max_monad:
                result[j] = min(9 - diff, 9)
                result[i] = min(9 + diff, 9)
            else:
                result[j] = max(1 - diff, 1)
                result[i] = max(1 + diff, 1)

    return result


def part1(inp):
    X = [int(inp[i][2]) for i in range(5, len(inp), 18)]
    W = [int(inp[i][2]) for i in range(4, len(inp), 18)]
    Y = [int(inp[i][2]) for i in range(15, len(inp), 18)]
    max_model = find_model(X, W, Y, True)
    assert(execute(inp, max_model[:]) == 0)

    return ''.join(map(str, max_model))


def part2(inp):
    X = [int(inp[i][2]) for i in range(5, len(inp), 18)]
    W = [int(inp[i][2]) for i in range(4, len(inp), 18)]
    Y = [int(inp[i][2]) for i in range(15, len(inp), 18)]
    min_model = find_model(X, W, Y, False)
    assert(execute(inp, min_model[:]) == 0)

    return ''.join(map(str, min_model))


def main():
    with open(f'{dir_path}/../../inputs/day24/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

