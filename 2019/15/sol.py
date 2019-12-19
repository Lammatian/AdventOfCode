import sys
sys.path.append('..')
import os
from enum import Enum
from util.intcode import Intcode

from time import sleep

BOARD_SIZE = 50

class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def right(self):
        return Direction((self.value + 1) % 4)

    def left(self):
        return Direction((self.value - 1) % 4)


def go_forward(direction):
    if direction == Direction.UP:
        return parse_input('w')
    elif direction == Direction.RIGHT:
        return parse_input('d')
    elif direction == Direction.DOWN:
        return parse_input('s')
    elif direction == Direction.LEFT:
        return parse_input('a')
    else:
        return 0


def print_board(board, droid=None, oxygen=None, width=1):
    if oxygen is not None:
        board[oxygen[1]][oxygen[0]] = 'O'

    if droid is not None:
        board[droid[1]][droid[0]] = 'D'

    fmt = str(r'{: ^' + str(width) + r'}')
    board_str = '\n'.join(map(lambda x: ''.join(map(lambda s: fmt.format(s), x)), board))

    print(board_str)


def move_droid(start, direction):
    dirs = {
        1: (0, 1),  # North
        2: (0, -1), # South
        3: (-1, 0), # West
        4: (1, 0)   # East
    }

    move = dirs[direction]

    return start[0] + move[0], start[1] + move[1]


def parse_input(direction):
    directions = {
        'w': 2,
        'a': 3,
        's': 1,
        'd': 4
    }

    return directions.get(direction, 1)


def sol1(data, show=False):
    computer = Intcode(data)
    board = [None] * BOARD_SIZE
    dist = [None] * BOARD_SIZE
    
    for i in range(BOARD_SIZE):
        board[i] = [' '] * BOARD_SIZE
        dist[i] = [-1] * BOARD_SIZE

    d_x, d_y = BOARD_SIZE // 2, BOARD_SIZE // 2
    board[d_y][d_x] = 'D'
    dist[d_y][d_x] = 0
    last_inp = None
    oxygen = None
    cur_dir = Direction.UP
    cur_dist = 0

    moves = 0
    while (d_x, d_y) != (BOARD_SIZE // 2, BOARD_SIZE // 2) or moves == 0:
        out = computer.execute_command()

        if out is not None:
            if out >= 0:
                if out == 0:
                    w_x, w_y = move_droid((d_x, d_y), last_inp)
                    board[w_y][w_x] = '#'
                    # Hit wall, adjust to the right twice
                    cur_dir = cur_dir.right()
                    cur_dir = cur_dir.right()
                elif out == 1:
                    if moves == 0:
                        moves = 1
                    board[d_y][d_x] = '.' # Mark as safe
                    d_x, d_y = move_droid((d_x, d_y), last_inp) 

                    if dist[d_y][d_x] >= 0:
                        cur_dist = dist[d_y][d_x]
                    else:
                        cur_dist += 1
                        dist[d_y][d_x] = cur_dist
                elif out == 2:
                    d_x, d_y = move_droid((d_x, d_y), last_inp) 
                    oxygen = (d_x, d_y)
                    cur_dist += 1 # definitely wasn't there before, just increase
                    return cur_dist

                if show:
                    print_board(board, (d_x, d_y), oxygen)
                    print('Current distance:', cur_dist)
                    sleep(0.005)
            elif out == -2: # waiting for input
                cur_dir = cur_dir.left()
                last_inp = go_forward(cur_dir)
                computer.add_input(last_inp)

    return cur_dist


def retrieve_board(data, show=False):
    """
    Retrieve the whole board by sticking to the left wall
    until reaching the beginning
    """
    if show:
        print('Retrieving board...')

    computer = Intcode(data) # Otherwise the default list argument is overwritten
    board = [None] * BOARD_SIZE
    
    for i in range(BOARD_SIZE):
        board[i] = [' '] * BOARD_SIZE

    d_x, d_y = BOARD_SIZE // 2, BOARD_SIZE // 2
    board[d_y][d_x] = 'D'
    last_inp = None
    cur_dir = Direction.UP
    oxygen = None

    moves = 0
    while (d_x, d_y) != (BOARD_SIZE // 2, BOARD_SIZE // 2) or moves == 0:
        out = computer.execute_command()

        if out is not None:
            if out >= 0:
                if out == 0:
                    w_x, w_y = move_droid((d_x, d_y), last_inp)
                    board[w_y][w_x] = '#'
                    # Hit wall, adjust to the right twice
                    cur_dir = cur_dir.right()
                    cur_dir = cur_dir.right()
                elif out == 1:
                    if moves == 0:
                        moves = 1
                    board[d_y][d_x] = '.' # Mark as safe
                    d_x, d_y = move_droid((d_x, d_y), last_inp) 
                elif out == 2:
                    d_x, d_y = move_droid((d_x, d_y), last_inp) 
                    oxygen = (d_x, d_y)

                if show:
                    print_board(board, (d_x, d_y), oxygen)
                    sleep(0.01)
            elif out == -2: # waiting for input
                cur_dir = cur_dir.left()
                last_inp = go_forward(cur_dir)
                computer.add_input(last_inp)

    if show:
        print('Board retrieved')

    return board, oxygen


def sol2(data, show=False):
    board, oxygen = retrieve_board(data, show)
    # BFS
    queue = [(oxygen, 0)]
    max_dist = 0

    while queue:
        point, distance = queue.pop(0)
        c_x, c_y = point

        max_dist = max(max_dist, distance)

        for (x, y) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            n_x, n_y = c_x + x, c_y + y
            if board[n_y][n_x] == '.': # Not yet visited
                board[n_y][n_x] = str(distance + 1)
                queue.append(((n_x, n_y), distance + 1))

    if show:
        print_board(board, None, oxygen, width=5)

    return max_dist


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()