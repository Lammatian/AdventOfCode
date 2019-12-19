import sys
sys.path.append('..')
import os
from enum import Enum
from util.intcode import Intcode

from time import sleep

def sol1(code):
    computer = Intcode(code)

    blocks = 0

    while not computer.done():
        computer.execute_command()

        if len(computer.outputs) == 3:
            x, y, tile = computer.outputs
            computer.outputs = []

            if tile == 2:
                blocks += 1

    return blocks


def get_dimensions(blocks):
    max_x = 0
    max_y = 0

    for x, y, _ in blocks:
        max_x = max(max_x, x)
        max_y = max(max_y, y)

    return (max_x, max_y)


def simulate_board(blocks, width, height, display=False):
    """
    Simulate board and return score and best move
    """
    block_to_char = {
        0: ' ',
        1: '|',
        2: '#',
        3: '=',
        4: 'o',
    }

    if display:
        board = [None] * (height + 1)

        for i in range(height + 1):
            board[i] = [' '] * (width + 1)

    score = 0
    ball_x = 0
    padd_x = 0

    for x, y, b in blocks:
        if x == -1 and y == 0:
            score = b
            continue
        
        if b == 3:
            padd_x = x
        if b == 4:
            ball_x = x

        if display:
            board[y][x] = block_to_char[b]

    if display:
        os.system('clear')
        print('\nSCORE: {}\n'.format(score))
        print('\n'.join(list(map(lambda x: ''.join(x), board))))

    move = 0

    if padd_x < ball_x:
        move = 1
    elif padd_x > ball_x:
        move = -1

    return score, move


def sol2(code, user_input=False, display=False):
    code[0] = 2
    computer = Intcode(code, user_input=user_input)

    blocks = []
    score = 0

    while not computer.done():
        if computer.is_input():
            score, move = simulate_board(blocks, *get_dimensions(blocks), display)
            
            if not user_input:
                computer.add_input(move)

        computer.execute_command()

        if len(computer.outputs) == 3:
            x, y, tile = computer.outputs
            blocks.append((x, y, tile))

            computer.outputs = []

    score, _ = simulate_board(blocks, *get_dimensions(blocks), display)

    return score


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()