import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    i = 1
    while i*i < inp:
        i += 2
    to_outer = i//2
    start = i*i
    while start - i + 1 > inp:
        start -= i - 1
    start, end = start - i + 1, start
    mid = end - (end - start) // 2
    to_mid = inp - mid
    return to_outer + to_mid


def part2(inp):
    S = 30
    board = []
    for _ in range(S):
        board.append([0] * S)

    x, y = S//2, S//2
    board[y][x] = 1
    dir_change_in = 0
    last_dir_change_in = 0
    second_same = False
    cur_dir = 0
    dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)]
    while True:
        x += dirs[cur_dir][0]
        y += dirs[cur_dir][1]
        next_val = sum(board[y + dy][x + dx] for dy in [-1, 0, 1] for dx in [-1, 0, 1])
        if next_val > inp:
            return next_val
        board[y][x] = next_val
        if dir_change_in == 0:
            cur_dir = (cur_dir + 1) % 4
            if second_same:
                dir_change_in = last_dir_change_in + 1
                last_dir_change_in += 1
                second_same = False
            else:
                dir_change_in = last_dir_change_in
                second_same = True
        else:
            dir_change_in -= 1
        for row in board:
            print(' '.join(list(map(lambda x: str(x).rjust(6), row))))
        print()


def main():
    with open(f'{dir_path}/../../inputs/day03/input') as f:
        inp = int(list(map(lambda x: x, f.read().strip().split('\n')))[0])

    print(inp)

    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

