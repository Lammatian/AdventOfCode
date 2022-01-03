import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    result = ''
    pos = (1, 1)
    for line in inp:
        for c in line:
            px, py = pos
            if c == 'L':
                pos = (max(0, px - 1), py)
            elif c == 'R':
                pos = (min(2, px + 1), py)
            elif c == 'U':
                pos = (px, max(0, py - 1))
            elif c == 'D':
                pos = (px, min(2, py + 1))
        result += str(pos[1] * 3 + pos[0] + 1)
        
    return result


def dist(pos):
    return sum(map(abs, pos))


def part2(inp):
    result = ''
    pos = (-2, 0)
    for line in inp:
        for c in line:
            px, py = pos
            if c == 'L':
                new_pos = (max(-2, px - 1), py)
            elif c == 'R':
                new_pos = (min(2, px + 1), py)
            elif c == 'U':
                new_pos = (px, max(-2, py - 1))
            elif c == 'D':
                new_pos = (px, min(2, py + 1))

            if dist(new_pos) <= 2:
                pos = new_pos

        if pos[1] == -2:
            result += '1'
        elif pos[1] == -1:
            result += str(3 + pos[0])
        elif pos[1] == 0:
            result += str(7 + pos[0])
        elif pos[1] == 1:
            result += 'ABC'[1 + pos[0]]
        else:
            result += 'D'
        
    return result


def main():
    with open(f'{dir_path}/../../inputs/day02/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

