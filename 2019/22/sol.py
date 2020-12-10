import sys
import numpy as np
from enum import Enum


class Op(Enum):
    REVERSE = 0
    CUT = 1
    INCREMENT = 2


def parse(cmd):
    if cmd[0] == 'c':
        return (Op.CUT, int(cmd.split()[-1]))
    elif cmd[-1] == 'k':
        return (Op.REVERSE, None)
    else:
        return (Op.INCREMENT, int(cmd.split()[-1]))


def sol1(commands, size=10007, target=2019):
    return (commands[0][1] * target - commands[1][1]) % size


def reduce_commands(commands, size):
    inc_pos = len(commands) - 1 

    while commands[inc_pos][0] != Op.INCREMENT:
        inc_pos -= 1

    while inc_pos > 0:
        cur_cmd = commands[inc_pos]
        prev_cmd = commands[inc_pos - 1]

        if prev_cmd[0] == Op.INCREMENT:
            # join increments
            commands[inc_pos - 1] = (Op.INCREMENT, (cur_cmd[1] * prev_cmd[1]) % size)
            commands.pop(inc_pos)
        elif prev_cmd[0] == Op.CUT:
            # swap increment<->cut
            commands[inc_pos - 1] = (Op.INCREMENT, cur_cmd[1])
            commands[inc_pos] = (Op.CUT, (cur_cmd[1] * prev_cmd[1]) % size)
        else:
            # change rev, inc into inc, cut
            commands[inc_pos - 1] = (Op.INCREMENT, -cur_cmd[1] % size)
            commands[inc_pos] = (Op.CUT, cur_cmd[1])

        inc_pos -= 1

    cut_pos = len(commands) - 1

    while commands[cut_pos][0] != Op.CUT:
        cut_pos -= 1

    # First command now is increment, we want cut to be second
    while cut_pos > 1:
        cur_cmd = commands[cut_pos]
        prev_cmd = commands[cut_pos - 1]

        if prev_cmd[0] == Op.CUT:
            # join cuts
            commands[cut_pos - 1] = (Op.CUT, (cur_cmd[1] + prev_cmd[1]) % size)
            commands.pop(cut_pos)
        else:
            # swap rev<->cut
            commands[cut_pos - 1] = (Op.CUT, -cur_cmd[1] % size)
            commands[cut_pos] = (Op.REVERSE, None)

        cut_pos -= 1

    while len(commands) > 3:
        # remove two consecutive revs as they don't do anything
        commands.pop()
        commands.pop()

    return commands


def fast_mod_exp(base, exp, mod):
    memo = {}
    # base^1 =  base
    memo[1] = base

    n = 2

    while n <= exp:
        memo[n] = memo[n//2]**2 % mod
        n *= 2

    result = 1
    n = 1
    while exp > 0:
        if exp % 2 == 1:
            result = (result * memo[n]) % mod

        n *= 2
        exp //= 2
    
    return result


def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)


def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m


def sol2(commands, size, loops, target_pos):
    # we know increment(a) is at the top, cut(b) at the bottom
    # if we call `loops` L, then the shift of increments to the top will result
    # in increment(a^L)cut(b(a^(L-1) + a^(L-2) + ... + a^0))
    inc_val = fast_mod_exp(commands[0][1], loops, size)
    cut_val = (commands[1][1] * ((inc_val - 1) * modinv(commands[0][1] - 1, size))) % size
    i = (modinv(inc_val, size) * (target_pos + cut_val)) % size

    return (modinv(inc_val, size) * (target_pos + cut_val)) % size


def main():
    SMALL_DECK = 10007
    SMALL_TARGET = 2019
    BIG_DECK = 119315717514047
    BIG_TARGET = 2020
    BIG_LOOP = 101741582076661

    with open(sys.argv[1]) as f:
        commands = list(map(parse, f.read().splitlines()))

    # solution 1
    print(sol1(reduce_commands(commands[:], SMALL_DECK), SMALL_DECK, SMALL_TARGET))
    # solution 2
    print(sol2(reduce_commands(commands[:], BIG_DECK), BIG_DECK, BIG_LOOP, BIG_TARGET))


if __name__ == '__main__':
    main()