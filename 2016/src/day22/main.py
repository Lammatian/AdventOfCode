import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    count = 0
    for n1 in inp:
        for n2 in inp:
            if n1 != n2:
                if n1[2] != 0 and n1[2] <= n2[3]:
                    count += 1

    return count


def neighbours(r, c, R, C):
    DR = [-1, 1, 0, 0]
    DC = [0, 0, -1, 1]
    ns = []

    for dr, dc in zip(DR, DC):
        r_ = r + dr
        c_ = c + dc

        if 0 <= r_ < R and 0 <= c_ < C:
            ns.append([r_, c_])

    return ns


def total(grid, r, c):
    return grid[r][c][0]


def used(grid, r, c):
    return grid[r][c][1]


def available(grid, r, c):
    return grid[r][c][2]


def part2(inp):
    # This is easier manually
    # We assume the input has a 'wall' of unmovable units
    # that separates the empty spot and the 'goal data'.
    # Once we get to the goal data, we just do 5-step loops
    # right->up->up->left->down that move the 'goal data'
    # a space up.
    # This solution works by calculating the height of the
    # 'wall' so that we can get the number of steps to get
    # to the 'goal' and then the number of loops we have to
    # perform
    # We assume all moves are possible (and they should be)
    R = max(x for (x, _), _, _, _, _ in inp)
    C = max(y for (_, y), _, _, _, _ in inp)

    grid = [[None for _ in range(C + 1)] for _ in range(R + 1)]
    for (r, c), t, u, a, _ in inp:
        grid[r][c] = (t, u, a)

    wall_r = R + 1  # no wall
    wall_c = -1  # no wall
    empty_r, empty_c = None, None
    for r in range(R):
        for c in range(C):
            if grid[r][c][0] > 100:
                wall_r = min(wall_r, r)
                wall_c = c
            if grid[r][c][1] == 0:
                empty_r, empty_c = r, c

    moves_over_wall = (empty_c - wall_c) + (empty_r - wall_r + 1)
    moves_to_y0 = wall_c
    moves_to_xmax = R - wall_r + 1
    moves_in_loops = 5 * (R - 1)
    return moves_over_wall + moves_to_y0 + moves_to_xmax + moves_in_loops


def parse(line):
    line = line.split()
    line[0] = line[0].split('-')[1:]
    line[0][0] = int(line[0][0][1:])
    line[0][1] = int(line[0][1][1:])
    for i in range(1, len(line)):
        line[i] = int(line[i][:-1])
    return line


def main():
    with open(f'{dir_path}/../../inputs/day22/input') as f:
        inp = list(map(parse, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

