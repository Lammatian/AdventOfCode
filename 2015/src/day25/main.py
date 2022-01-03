import os
dir_path = os.path.dirname(os.path.realpath(__file__))

MUL = 252533
DIV = 33554393
START = 20151125

def part1(row, col):
    diagonal = row + col - 1
    iterations = (diagonal * (diagonal + 1) // 2) - (diagonal - col) - 1
    cur = START

    for i in range(iterations):
        cur = (cur * MUL) % DIV
    return cur


def main():
    with open(f'{dir_path}/../../inputs/day25/input') as f:
        inp = f.read().strip().split()
        inp = (int(inp[-3][:-1]), int(inp[-1][:-1]))

    print(inp)
    
    print(part1(*inp))


if __name__ == '__main__':
    main()

