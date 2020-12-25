import sys
sys.path.append('..')
from util.intcode import Intcode


def sol1(ins):
    ins[1] = 12
    ins[2] = 2
    comp = Intcode(ins)
    comp.run(debug=False)

    return comp.ins[0]


def sol2(ins):
    for noun in range(100):
        for verb in range(100):
            new_ins = ins[:]
            new_ins[1] = noun
            new_ins[2] = verb

            comp = Intcode(new_ins)
            comp.run()

            if comp.ins[0] == 19690720:
                return 100 * noun + verb


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        ins = list(map(int, f.read().strip().split(','))) 

    print(sol1(ins[:]))
    print(sol2(ins[:]))
