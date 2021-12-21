import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import numpy as np
from collections import defaultdict


def part1(p):
    s = [0, 0]
    rolls = np.array([1, 2, 3])
    turn = 1
    while max(s) < 1000:
        i = 0 if turn % 2 == 1 else 1

        p[i] = (p[i] + sum(rolls))
        if p[i] > 10:
            p[i] %= 10
            if p[i] == 0:
                p[i] = 10

        s[i] += p[i]
        turn += 1
        rolls += 3
        for i, roll in enumerate(rolls):
            if roll > 100:
                rolls[i] %= 100

    return 3 * (turn - 1) * min(s)


def calculate_outcomes():
    result = defaultdict(int)
    for a in range(1, 4):
        for b in range(1, 4):
            for c in range(1, 4):
                result[a + b + c] += 1
    return result

outcomes = calculate_outcomes()

def normalize(p):
    return p % 10 if p % 10 != 0 else 10


MAX_SCORE = 21
def dp(mem, s, p, t, player):
    global outcomes
    if s in mem and p in mem[s] and t in mem[s][p]:
        return mem[s][p][t]
    elif s[player] >= MAX_SCORE:
            return 1
    elif s[(player + 1) % 2] >= MAX_SCORE:
            return 0

    result = 0
    for k, v in outcomes.items():
        if t == 0:
            p_ = (normalize(p[0] + k), p[1])
            s_ = (s[0] + p_[0], s[1])
            result += v * dp(mem, s_, p_, 1, player)
        else:
            p_ = (p[0], normalize(p[1] + k))
            s_ = (s[0], s[1] + p_[1])
            result += v * dp(mem, s_, p_, 0, player)

    if s in mem and p in mem[s]:
        mem[s][p][t] = result
    elif s in mem:
        mem[s][p] = {t: result}
    else:
        mem[s] = {p: {t: result}}

    return result


def part2(p):
    mem = {}
    wins1 = dp(mem, (0, 0), p, 0, 0)
    wins2 = dp(mem, (0, 0), p, 0, 1)
    return max(wins1, wins2)


def main():
    with open(f'{dir_path}/../../inputs/day21/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    p1 = int(inp[0].split()[-1])
    p2 = int(inp[1].split()[-1])
    print(inp, p1, p2)
    
    print(part1([p1, p2]))
    print(part2((p1, p2)))


if __name__ == '__main__':
    main()

