import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    X = 50
    Y = 50
    grid = [[' ' for _ in range(X)] for _ in range(Y)]
    for x in range(X):
        for y in range(Y):
            val = x*x + 3*x + 2*x*y + y + y*y + inp
            bin_val = "{0:b}".format(val)
            if bin_val.count('1') % 2 == 0:
                grid[y][x] = '.'
            else:
                grid[y][x] = '#'

    gx, gy = 31, 39
    seen = set([(0, 0)])
    queue = [((1, 1), 0, [])]
    while queue:
        (x, y), steps, prev = queue.pop(0)

        DX = [-1, 0, 1, 0]
        DY = [0, 1, 0, -1]

        for dx, dy in zip(DX, DY):
            x_ = x + dx
            y_ = y + dy
            if x_ == gx and y_ == gy:
                return steps + 1
            if 0 <= x_ < X and 0 <= y_ < Y and grid[y_][x_] == '.' and (x_, y_) not in seen:
                queue.append(((x_, y_), steps + 1, prev + [(x_, y_)]))
                seen.add((x_, y_))


def part2(inp):
    X = 51
    Y = 51
    grid = [[' ' for _ in range(X)] for _ in range(Y)]
    for x in range(X):
        for y in range(Y):
            val = x*x + 3*x + 2*x*y + y + y*y + inp
            bin_val = "{0:b}".format(val)
            if bin_val.count('1') % 2 == 0:
                grid[y][x] = '.'
            else:
                grid[y][x] = '#'

    seen = set([(1, 1)])
    queue = [((1, 1), 0, [])]
    while queue:
        (x, y), steps, prev = queue.pop(0)
        if steps >= 50:
            continue

        DX = [-1, 0, 1, 0]
        DY = [0, 1, 0, -1]

        for dx, dy in zip(DX, DY):
            x_ = x + dx
            y_ = y + dy
            if 0 <= x_ < X and 0 <= y_ < Y and grid[y_][x_] == '.' and (x_, y_) not in seen:
                queue.append(((x_, y_), steps + 1, prev + [(x_, y_)]))
                seen.add((x_, y_))

    return len(seen)


def main():
    with open(f'{dir_path}/../../inputs/day13/input') as f:
        inp = int(list(map(lambda x: x, f.read().strip().split('\n')))[0])

    print(inp)
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

