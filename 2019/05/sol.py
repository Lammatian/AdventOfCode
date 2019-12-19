import sys
sys.path.append('..')
from util.intcode import Intcode

def sol1(data):
    computer = Intcode(data, [1])
    computer.simulate()

    return computer.outputs[-1]


def sol2(data):
    computer = Intcode(data, [5])
    computer.simulate()

    return computer.outputs[-1]


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()