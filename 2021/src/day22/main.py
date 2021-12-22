import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import combinations
from collections import defaultdict


def part1(inp):
    grid = [[[0 for _ in range(101)] for _ in range(101)] for _ in range(101)]

    for t, ((xs, xe), (ys, ye), (zs, ze)) in inp:
        if xs > 50 or xe < -50 or ys > 50 or ye < -50 or zs > 50 or ze < -50:
            continue

        for x in range(max(-50, xs), min(50, xe) + 1):
            for y in range(max(-50, ys), min(50, ye) + 1):
                for z in range(max(-50, zs), min(50, ze) + 1):
                    grid[z][y][x] = 1 if t == 'on' else 0

    result = 0
    for z in grid:
        for y in z:
            for x in y:
                result += x

    return result


def lines_overlapping(l1, l2):
    return l1[1] >= l2[0] and l1[0] <= l2[1]


def overlapping(step1, step2):
    x1, y1, z1 = step1
    x2, y2, z2 = step2
    return lines_overlapping(x1, x2) and lines_overlapping(y1, y2) and lines_overlapping(z1, z2)


def determine_overlaps(new_step, previous_steps):
    overlaps = []
    for step in previous_steps:
        if overlapping(new_step[1], step[1]):
            overlaps.append(step)

    return overlaps


def overlap(s1, s2):
    (xs1, xe1), (ys1, ye1), (zs1, ze1) = s1
    (xs2, xe2), (ys2, ye2), (zs2, ze2) = s2
    if not overlapping(s1, s2):
        return None
    else:
        xrange = [max(xs1, xs2), min(xe1, xe2)]
        yrange = [max(ys1, ys2), min(ye1, ye2)]
        zrange = [max(zs1, zs2), min(ze1, ze2)]
        return [xrange, yrange, zrange]


def all_overlapping(steps):
    if len(steps) == 1:
        return True

    return all(overlapping(s1, s2) for s1, s2 in combinations(steps, 2))


def overlap_all(steps):
    box = steps[0]

    for step in steps[1:]:
        box = overlap(box, step)
        if not box:
            return None

    return box


def area(s):
    (xs, xe), (ys, ye), (zs, ze) = s
    return (xe - xs + 1) *  (ye - ys + 1) * (ze - zs + 1)


# To perform a step, we look at the current cuboid, find all the previous
# cuboids that overlap with it and determine how many lights will be toggled by
# the addition of the current cuboid
#
# Consider the following 2D example:
#
# 11111
# 111443
# 1116652
# 1116652
#    2222
#    2222
#
# where 1 is C1, 2 is C2, 3 is C3, 4 is overlap(C1, C3), 5 is overlap(C2, C3)
# and 6 is overlap(C1, C2, C3). Assume the cuboids were added in order C1, C2,
# C3 and that we want to calculate the amount of lights toggled on by 3.
#
# Then we consider all possible combinations of cuboids that include C3 as
# follows:
#
# initialise all reductions to 0 and total change to 0
#
# overlap(C1, C2, C3) = area(C1, C2, C3) - reductions[(C1, C2, C3)] = 4 - 0 = 4
# reductions[(C1, C2)] += 4, total of 4
# reductions[(C1, C3)] += 4, total of 4
# reductions[(C2, C3)] += 4, total of 4
# reductions[C1] += 4, total of 4
# reductions[C2] += 4, total of 4
# reductions[C3] += 4, total of 4
# total_change is not affected as C2 already turned these on
#
# overlap(C1, C3) = area(C1, C3) - reductions[(C1, C3)] = 6 - 4 = 2
# reductions[C1] += 2, total of 6
# reductions[C3] += 2, total of 6
# total_change is not affected as C1 already turned these on
#
# overlap(C2, C3) = area(C2, C3) - reductions[(C2, C3)] = 6 - 4 = 2
# reductions[C2] += 2, total of 6
# reductions[C3] += 2, total of 8
# total_change is not affected as C2 already turned these on
#
# overlap(C3) = area(C3) - reductions[C3] = 9 - 8 = 1
# total_change += overlap(C3), total of 1
#
# Thus 1 additional light was turned on
# 
# Now consider the same example, except now C3 turns the lights off:
#
# 11111
# 111   
# 111   2
# 111   2
#    2222
#    2222
#
# We would perform the following steps to calculate how many lights were turned
# off:
#
# initialise all reductions to 0 and total change to 0
#
# overlap(C1, C2, C3) = area(C1, C2, C3) - reductions[(C1, C2, C3)] = 4 - 0 = 4
# reductions[(C1, C2)] += 4, total of 4
# reductions[(C1, C3)] += 4, total of 4
# reductions[(C2, C3)] += 4, total of 4
# reductions[C1] += 4, total of 4
# reductions[C2] += 4, total of 4
# reductions[C3] += 4, total of 4
# total_change += overlap(C1, C2, C3), total of 4
#
# overlap(C1, C3) = area(C1, C3) - reductions[(C1, C3)] = 6 - 4 = 2
# reductions[C1] += 2, total of 6
# reductions[C3] += 2, total of 6
# total_change += overlap(C1, C3), total of 6
#
# overlap(C2, C3) = area(C2, C3) - reductions[(C2, C3)] = 6 - 4 = 2
# reductions[C2] += 2, total of 6
# reductions[C3] += 2, total of 8
# total_change += overlap(C2, C3), total of 8
#
# overlap(C3) = area(C3) - reductions[C3] = 9 - 8 = 1
# total_change is not affected as this light was already off
#
# Thus 8 lights were turned off
def perform_step(inp, step):
    step_change = 0
    cur_step = inp[step]
    prev_steps = inp[:step]
    overlaps = determine_overlaps(cur_step, prev_steps)
    reductions = defaultdict(int)

    for N in range(len(overlaps), -1, -1):
        combos = combinations(overlaps, N) if overlaps else [()]
        for subset in combos:
            subset = subset + (cur_step,)
            subset_ = tuple((p for _, p in subset))
            full_overlap = overlap_all(subset_)
            if not full_overlap:
                continue

            overlap_area = area(full_overlap)
            change = overlap_area - reductions[subset_]
            if len(subset) == 1:
                if cur_step[0] == 'on':
                    step_change += change
            else:
                if subset[-2][0] == 'off' and cur_step[0] == 'on':
                    step_change += change
                elif subset[-2][0] == 'on' and cur_step[0] == 'off':
                    step_change -= change 

            for M in range(len(subset) - 1, 0, -1):
                for ssubset_ in combinations(subset_, M):
                    reductions[ssubset_] += change

    return step_change
                

def part2(inp):
    total_on = 0
    for i in range(len(inp)):
        total_on += perform_step(inp, i)

    return total_on


def main():
    with open(f'{dir_path}/../../inputs/day22/input') as f:
        inp = list(map(lambda x: (x.split()[0], x.split()[1].split(',')), f.read().strip().split('\n')))

    for i in range(len(inp)):
        toggle, cds = inp[i]
        inp[i] = (toggle, tuple(map(lambda x: tuple(map(int, tuple(x[2:].split('..')))), cds)))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

