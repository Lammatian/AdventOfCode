import sys
sys.path.append('..')
from util.intcode import Intcode

def sol1(ins):
    comp = Intcode(ins, [1])
    out = comp.simulate()

    return out[0]


def sol2(ins):
    comp = Intcode(ins, [2])
    out = comp.simulate()

    return out[0]


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()