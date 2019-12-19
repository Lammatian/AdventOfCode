import sys
sys.path.append('..')
from enum import Enum
from util.intcode import Intcode

from time import sleep

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


def sol2(code, show_progress=False):
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

        if show_progress:
            print(identifier(colour))
            print()
            sleep(0.05)

    return identifier(colour)


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()