import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(X, Y):
    xs, xe = X
    ys, ye = Y
    best_height = -1

    for _dy in range(300):
        for _dx in range(100):
            dx = _dx
            dy = _dy
            x, y = 0, 0
            max_y = 0
            while x <= xe and y >= ys:
                x += dx
                y += dy
                max_y = max(max_y, y)
                dx -= 1 if dx > 0 else 0
                dy -= 1
                
                if xs <= x <= xe and ys <= y <= ye:
                    best_height = max(max_y, best_height)
                    break

    return best_height


def part2(X, Y):
    xs, xe = X
    ys, ye = Y
    in_range = 0

    for _dy in range(ys, 1000):
        for _dx in range(xe + 1):
            dx = _dx
            dy = _dy
            x, y = 0, 0
            max_y = 0
            while x <= xe and y >= ys:
                x += dx
                y += dy
                max_y = max(max_y, y)
                dx -= 1 if dx > 0 else 0
                dy -= 1
                
                if xs <= x <= xe and ys <= y <= ye:
                    in_range += 1
                    break

    return in_range


def main():
    with open(f'{dir_path}/../../inputs/day17/input') as f:
        inp = f.read().strip().split(': ')[1].split(', ')

    inp = list(map(lambda x: tuple(map(int, x[2:].split('..'))), inp))
    print(inp)
    
    print(part1(*inp[:]))
    print(part2(*inp[:]))


if __name__ == '__main__':
    main()

