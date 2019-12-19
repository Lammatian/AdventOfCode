import sys
import os
from enum import Enum
from collections import defaultdict


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

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()