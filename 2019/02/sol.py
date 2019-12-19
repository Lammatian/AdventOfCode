import sys
sys.path.append('..')
from util.intcode import Intcode

def sol1(data):
    data[1] = 12
    data[2] = 2

    computer = Intcode(data)
    computer.simulate()

    return computer.ins[0]


def sol2(data, target):
    for a in range(100):
        for b in range(100):
            custom_data = data[:]
            custom_data[1] = a
            custom_data[2] = b

            computer = Intcode(custom_data)
            computer.simulate()

            if computer.ins[0] == target:
                return 100*a + b


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:], 19690720))


if __name__ == '__main__':
    main()