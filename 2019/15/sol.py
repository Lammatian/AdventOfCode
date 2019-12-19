import sys
import os
from enum import Enum
from collections import defaultdict

from time import sleep

BOARD_SIZE = 50

class Intcode:
    def __init__(self, ins, inputs=None, user_input=False):
        self.ins = defaultdict(int)
        self.user_input = user_input

        for i, inst in enumerate(ins):
            self.ins[i] = inst

        self.cur = 0

        if inputs is None:
            self.inputs = []
        else:
            self.inputs = inputs

        self.cur_inp = 0
        self.rel_add = 0

        self.outputs = []

    def simulate(self):
        while self.cur >= 0:
            self.cur = self.parse_command()

        return self.outputs

    def done(self):
        return self.cur < 0

    def is_input(self):
        return self.ins[self.cur] % 100 == 3

    def execute_command(self):
        """
        Parse and execute the command, updating current pointer
        If command produced an output, return it

        If waiting for input, return -2
        """
        last_cur = self.cur
        out_len = len(self.outputs)
        self.cur = self.parse_command()
        
        if len(self.outputs) > out_len:
            return self.outputs[-1]
        elif not self.user_input and last_cur == self.cur:
            return -2

    def add_input(self, new_input):
        self.inputs.append(new_input)

    def parse_command(self):
        """
        Parse command and return next position
        If no next position, return -1
        """
        op_code = self.ins[self.cur]
        op = op_code % 100
        modes = [self.digit(op_code, i) for i in range(2, 5)]

        if op < 3: # add/mul
            self.print_current_cmd(3)
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, modes[2], True)
            result = a1 + a2 if op == 1 else a1 * a2
            self.ins[a3] = result
            return self.cur + 4
        elif op == 3: # input
            self.print_current_cmd(1)
            if self.cur_inp >= len(self.inputs):
                if self.user_input:
                    while True:
                        try:
                            move = int(input('Give input: '))
                            break
                        except ValueError:
                            continue

                    self.inputs.append(move)
                else:
                    return self.cur

            a1 = self.get_val(self.cur + 1, modes[0], True)
            self.ins[a1] = self.inputs[self.cur_inp]
            self.cur_inp += 1
            return self.cur + 2
        elif op == 4: # output
            self.print_current_cmd(1)
            a1 = self.get_val(self.cur + 1, modes[0])
            self.outputs.append(a1)
            return self.cur + 2
        elif op == 5: # jump if true
            self.print_current_cmd(2)
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])

            if a1 != 0:
                return a2
            
            return self.cur + 3
        elif op == 6: # jump if false
            self.print_current_cmd(2)
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])

            if a1 == 0:
                return a2
            
            return self.cur + 3
        elif op == 7: # set less than
            self.print_current_cmd(3)
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, modes[2], True)

            if a1 < a2:
                self.ins[a3] = 1
            else:
                self.ins[a3] = 0

            return self.cur + 4
        elif op == 8: # set equals
            self.print_current_cmd(3)
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, modes[2], True)

            if a1 == a2:
                self.ins[a3] = 1
            else:
                self.ins[a3] = 0

            return self.cur + 4
        elif op == 9: # update relative address
            self.print_current_cmd(1)
            a1 = self.get_val(self.cur + 1, modes[0])

            self.rel_add += a1

            return self.cur + 2
        else: # 99 = end
            return -1

    def print_current_cmd(self, args):
        """
        Print the current command and all its `args`
        arguments
        """
        return
        print(self.cur,
              self.rel_add,
              [self.ins[i] for i in range(self.cur, self.cur + args + 1)],
              self.ins[1000])
    
    def get_val(self, pos, mode, is_read=False):
        """
        Get argument value based on the mode
        If mode = 0, gets ins[ins[pos]]
        If mode = 1, gets ins[pos]
        If mode = 2, gets ins[rel_add + ins[pos]]
        """
        if not is_read:
            if mode == 0:
                return self.ins[self.ins[pos]]
            elif mode == 1:
                return self.ins[pos]
            elif mode == 2:
                return self.ins[self.ins[pos] + self.rel_add]
        else:
            if mode == 0:
                return self.ins[pos]
            elif mode == 2:
                return self.ins[pos] + self.rel_add

    @staticmethod
    def digit(n, i):
        """
        Get i-th digit of n
        """
        if i >= len(str(n)):
            return 0

        return int(str(n)[-(i + 1)])


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

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()