import sys
sys.path.append('..')
import os
from enum import Enum
from util.intcode import Intcode

def sol1(data):
    computer = Intcode(data)
    board = [[]]

    while not computer.done():
        computer.execute_command()

        if computer.outputs:
            out = computer.outputs.pop()

            if out == 10:
                board.append([])
            else:
                board[-1].append(chr(out))

    while not board[-1]:
        board.pop()

    result = 0

    for y, row in enumerate(board):
        if y == 0 or y == len(board) - 1:
            continue

        for x, c in enumerate(row):
            if c != '#' or x == 0 or x == len(row) - 1:
                continue

            neighbours = 0

            for nx, ny in [(x-1, y), (x+1, y), (x, y+1), (x, y-1)]:
                if board[ny][nx] == '#':
                    neighbours += 1

            if neighbours == 4:
                result += x * y

    return result


def sol2(data, feed=False):
    """
    Manually computed input:
    ABABCBACBC
    A = L 12 L 8 R 10 R 10
    B = L 6 L 4 L 12
    C = R 10 L 8 L 4 R 10
    """
    # with open('board.txt', 'w') as f:
    #     for row in board:
    #         f.write(row.__str__() + '\n')
    data[0] = 2
    main_routine = 'A,B,A,B,C,B,A,C,B,C'
    routine_a = 'L,12,L,8,R,10,R,10'
    routine_b = 'L,6,L,4,L,12'
    routine_c = 'R,10,L,8,L,4,R,10'
    feed_char = 'y' if feed else 'n'
    input_str = '\n'.join([main_routine, routine_a, routine_b, routine_c, feed_char]) + '\n'
    inputs = [ord(c) for c in input_str]
    computer = Intcode(data, inputs=inputs)

    while not computer.done():
        computer.execute_command()

        if computer.outputs[-2:] == [10, 10] and feed:
            print('\n' * 100)
            print(''.join([chr(i) for i in computer.outputs]), end='')
            computer.outputs = []

    return computer.outputs[-1]


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()