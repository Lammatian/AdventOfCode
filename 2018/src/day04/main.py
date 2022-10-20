import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import sys
from itertools import product, permutations, combinations
from functools import reduce
from collections import Counter, defaultdict
import numpy as np


def parse(line):
    line = line.split('] ')
    hour = int(line[0][-5:-3])
    minute = int(line[0][-2:])
    action = line[1]
    return (hour, minute, action)


def main():
    if len(sys.argv) > 1:
        filepath = f'{dir_path}/../../inputs/day04/' + sys.argv[1]
    else:
        filepath = f'{dir_path}/../../inputs/day04/input'

    with open(filepath) as f:
        inp = list(map(lambda x: parse(x), sorted(f.read().strip().split('\n'))))

    print(inp)

    print(part1(inp[:]))
    print(part2(inp[:]))


def part1(inp):
    sleep_times = defaultdict(int)
    curr = None
    sleep_start = None
    for h, m, a in inp:
        if a[0] == 'G':  # begins shift
            curr = int(a.split()[1][1:])
        elif a[0] == 'f':  # falls asleep
            sleep_start = m 
        elif a[0] == 'w':  # wakes up
            sleep_times[curr] += m - sleep_start

    minutes = [0] * 60
    guard, slept = max(sleep_times.items(), key=lambda x: x[1])
    right_guard = False
    for h, m, a in inp:
        if a[0] == 'G':
            curr = int(a.split()[1][1:])
            if curr != guard:
                right_guard = False
            else:
                right_guard = True
        if not right_guard:
            continue
        if a[0] == 'f':
            sleep_start = m
        elif a[0] == 'w':
            for i in range(sleep_start, m):
                minutes[i] += 1

    return minutes.index(max(minutes)) * guard


def part2(inp):
    sleep_times = {}
    curr = None
    sleep_start = None
    for h, m, a in inp:
        if a[0] == 'G':
            curr = int(a.split()[1][1:])
        elif a[0] == 'f':
            sleep_start = m
        elif a[0] == 'w':
            if curr not in sleep_times:
                sleep_times[curr] = [0] * 60
            for i in range(sleep_start, m):
                sleep_times[curr][i] += 1

    cur_max = 0
    cur_guard = -1
    cur_min = -1
    for k, mins in sleep_times.items():
        if max(mins) > cur_max:
            cur_guard = k
            cur_min = mins.index(max(mins)) 
            cur_max = max(mins)
    return cur_guard * cur_min


if __name__ == '__main__':
    main()

