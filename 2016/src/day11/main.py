import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import combinations
from copy import deepcopy


def done(setup):
    return len(setup[-1]) == sum(map(len, setup))


def generators(floor):
    return (elem for elem in floor if elem[0] == 'g')


def chips(floor):
    return (elem for elem in floor if elem[0] == 'c')


def c2g(chip):
    return 'g' + chip[1]


def possible(setup):
    for floor in setup:
        for c in chips(floor):
            if c2g(c) not in floor and list(generators(floor)):
                return False

    return True


def lolhash(setup, elev):
    result = [[None, None] for _ in range(sum(map(len, setup)) // 2)]
    for i, floor in enumerate(setup):
        for c in chips(floor):
            result[int(c[1]) - 1][0] = i
        for g in generators(floor):
            result[int(g[1]) - 1][1] = i

    return (tuple(map(tuple, sorted(result))), elev)


def solve(setup):
    start_elev = 0
    queue = [(setup, start_elev, 0)]
    seen = set([lolhash(setup, start_elev)])
    while queue:
        setup, elev, steps = queue.pop(0)
        if done(setup):
            return steps

        if elev < 3:
            for e1, e2 in combinations(setup[elev], 2):
                s_ = deepcopy(setup)
                s_[elev].remove(e1)
                s_[elev].remove(e2)
                s_[elev + 1] += [e1, e2]
                if possible(s_) and lolhash(s_, elev + 1) not in seen:
                    queue.append((s_, elev + 1, steps + 1))
                    seen.add(lolhash(s_, elev + 1))

            for e in setup[elev]:
                s_ = deepcopy(setup)
                s_[elev].remove(e)
                s_[elev + 1] += [e]
                if possible(s_) and lolhash(s_, elev + 1) not in seen:
                    queue.append((s_, elev + 1, steps + 1))
                    seen.add(lolhash(s_, elev + 1))

        if elev > 0 and sum(map(len, setup[:elev])) > 0:
            for e1, e2 in combinations(setup[elev], 2):
                s_ = deepcopy(setup)
                s_[elev].remove(e1)
                s_[elev].remove(e2)
                s_[elev - 1] += [e1, e2]
                if possible(s_) and lolhash(s_, elev + 1) not in seen:
                    queue.append((s_, elev - 1, steps + 1))
                    seen.add(lolhash(s_, elev - 1))

            for e in setup[elev]:
                s_ = deepcopy(setup)
                s_[elev].remove(e)
                s_[elev - 1] += [e]
                if possible(s_) and lolhash(s_, elev - 1) not in seen:
                    queue.append((s_, elev - 1, steps + 1))
                    seen.add(lolhash(s_, elev - 1))


def part1():
    setup = [
        ['g1', 'c1'],
        ['g2', 'g3', 'g4', 'g5'],
        ['c2', 'c3', 'c4', 'c5'],
        []
    ]
    return solve(setup)


def part2():
    setup = [
        ['g1', 'c1', 'g6', 'c6', 'g7', 'c7'],
        ['g2', 'g3', 'g4', 'g5'],
        ['c2', 'c3', 'c4', 'c5'],
        []
    ]
    return solve(setup)


def main():
    print(part1())
    print(part2())


if __name__ == '__main__':
    main()

