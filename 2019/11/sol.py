import sys
from enum import Enum
from collections import defaultdict

from time import sleep

class Intcode():
    def __init__(self, ins, inputs):
        self.ins = defaultdict(int)

        for i, inst in enumerate(ins):
            self.ins[i] = inst

        self.cur = 0

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

    def execute_command(self):
        """
        Parse and execute the command, updating current pointer
        If command produced an output, return it
        """
        out_len = len(self.outputs)
        self.cur = self.parse_command()
        
        if len(self.outputs) > out_len:
            return self.outputs[-1]

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


class Dir(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def succ(self):
        return Dir((self.value + 1) % 4)

    def pred(self):
        return Dir((self.value - 1) % 4)

    def xy(self):
        if self.value == 0:
            return (0, 1)
        elif self.value == 1:
            return (1, 0)
        elif self.value == 2:
            return (0, -1)
        elif self.value == 3:
            return (-1, 0)
        else:
            return (0, 0)


def sol1(code, dbg=False):
    colour = {}
    current_pos = [0, 0]
    current_dir = Dir(0)
    computer = Intcode(code, [])
    next_input = 0

    while not computer.done():
        computer.add_input(next_input)

        while not computer.done() and len(computer.outputs) < 2:
            computer.execute_command()

        if computer.done():
            break

        paint, d = computer.outputs
        current_dir = current_dir.pred() if d == 0 else current_dir.succ()
        computer.outputs = []

        colour[tuple(current_pos)] = paint
        current_pos[0] += current_dir.xy()[0]
        current_pos[1] += current_dir.xy()[1]
        next_input = colour.get(tuple(current_pos), 0)

    return len(colour) 


def identifier(colour):
    c_xmax = max([x for (x, y) in colour.keys()])
    c_xmin = min([x for (x, y) in colour.keys()])
    c_ymax = max([y for (x, y) in colour.keys()])
    c_ymin = min([y for (x, y) in colour.keys()])
    x_range = c_xmax - c_xmin
    y_range = c_ymax - c_ymin

    identifier = [None] * (y_range + 1)

    for i in range(y_range + 1):
        identifier[i] = [' '] * (x_range + 1)

    for pos, c in colour.items():
        if c == 1:
            identifier[-pos[1]][pos[0] - c_xmin] = '*'

    return '\n'.join(map(lambda x: ''.join(x), identifier))


def sol2(code):
    colour = {}
    current_pos = [0, 0]
    current_dir = Dir(0)
    computer = Intcode(code, [])
    next_input = 1

    while not computer.done():
        computer.add_input(next_input)

        while not computer.done() and len(computer.outputs) < 2:
            computer.execute_command()

        if computer.done():
            break

        paint, d = computer.outputs
        current_dir = current_dir.pred() if d == 0 else current_dir.succ()
        computer.outputs = []

        colour[tuple(current_pos)] = paint
        current_pos[0] += current_dir.xy()[0]
        current_pos[1] += current_dir.xy()[1]
        next_input = colour.get(tuple(current_pos), 0)

        # print(identifier(colour))
        # print()
        # sleep(0.05)

    return identifier(colour)


def main():
    with open(sys.argv[1]) as f:
        code = list(map(int, f.read().strip().split(',')))

    print(sol1(code))
    print(sol2(code))


if __name__ == '__main__':
    main()