import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    grid = [['.' for _ in range(50)] for _ in range(6)]
    for ins in inp:
        if ins[:4] == 'rect':
            W, H = list(map(int, ins[5:].split('x')))
            for r in range(W):
                for c in range(H):
                    grid[c][r] = '#'
        else:
            words = ins.split()
            if words[1] == 'row':
                c = int(words[2].split('=')[1])
                v = int(words[-1])
                grid[c] = grid[c][-v:] + grid[c][:-v]
            else:
                r = int(words[2].split('=')[1])
                v = int(words[-1])
                tmp_col = [grid[(c - v) % 6][r] for c in range(6)]
                for c in range(6):
                    grid[c][r] = tmp_col[c]

    result = 0
    for row in grid:
        result += row.count('#')
    return result


def part2(inp):
    grid = [['.' for _ in range(50)] for _ in range(6)]
    for ins in inp:
        if ins[:4] == 'rect':
            W, H = list(map(int, ins[5:].split('x')))
            for r in range(W):
                for c in range(H):
                    grid[c][r] = '#'
        else:
            words = ins.split()
            if words[1] == 'row':
                c = int(words[2].split('=')[1])
                v = int(words[-1])
                grid[c] = grid[c][-v:] + grid[c][:-v]
            else:
                r = int(words[2].split('=')[1])
                v = int(words[-1])
                tmp_col = [grid[(c - v) % 6][r] for c in range(6)]
                for c in range(6):
                    grid[c][r] = tmp_col[c]

    result = ''
    for row in grid:
        result += ''.join(row).replace('.', ' ') + '\n'

    return result


def main():
    with open(f'{dir_path}/../../inputs/day08/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

