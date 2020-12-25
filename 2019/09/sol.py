import sys
sys.path.append('..')
from util.intcode import Intcode


def sol1(ins):
    comp = Intcode(ins, inputs=[1])
    comp.run()

    return comp.outputs[0]


def sol2(ins):
    comp = Intcode(ins, inputs=[2])
    comp.run()

    return comp.outputs[0]


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        ins = list(map(int, f.read().strip().split(','))) 

    print(sol1(ins[:]))
    print(sol2(ins[:]))
