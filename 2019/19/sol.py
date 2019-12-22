import sys
sys.path.append('..')
from util.intcode import Intcode

def print_board(board):
    for row in board:
        print(''.join(row))
        

def sol1(data, show_board=False):
    if show_board:
        board = []

        for _ in range(50):
            board.append(['.'] * 50)

    result = 0
    for x in range(50):
        for y in range(50):
            computer = Intcode(data[:], [x, y])
            computer.simulate()

            if computer.outputs[-1] == 1:
                result += 1

                if show_board:
                    board[y][x] = '#'

    if show_board:
        print_board(board)

    return result 


def check_beam(data, x, y):
    computer = Intcode(data, [x, y])
    computer.simulate()

    return computer.outputs[-1]


def sol2(data):
    y = 10
    x = 0

    while check_beam(data, x, y) != 1:
        x += 1

    x -= 1

    while True:
        while check_beam(data, x + 1, y) != 0:
            x += 1

        if x < 100:
            y += 1
            continue

        # top-left
        if check_beam(data, x - 99, y) != 1:
            y += 1
            continue

        # bottom-left
        if check_beam(data, x - 99, y + 99) != 1:
            y += 1
            continue

        # bottom right
        if check_beam(data, x, y + 99) != 1:
            y += 1
            continue

        return 10000 * (x - 99) + y


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()