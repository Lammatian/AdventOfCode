import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    result = [inp]
    count = inp.count('.')
    last = inp
    for i in range(39):
        new_row = ''
        for i in range(len(last)):
            a = last[i - 1] if i - 1 >= 0 else '.'
            b = last[i]
            c = last[i + 1] if i + 1 < len(last) else '.'
            if a + b + c == '^^.':
                new_row += '^'
            elif a + b + c == '.^^':
                new_row += '^'
            elif a + b + c == '^..':
                new_row += '^'
            elif a + b + c == '..^':
                new_row += '^'
            else:
                new_row += '.'
                count += 1

        last = new_row

    return count


def part2(inp):
    result = [inp]
    count = inp.count('.')
    last = inp
    for i in range(399999):
        new_row = ''
        for i in range(len(last)):
            a = last[i - 1] if i - 1 >= 0 else '.'
            b = last[i]
            c = last[i + 1] if i + 1 < len(last) else '.'
            if a + b + c == '^^.':
                new_row += '^'
            elif a + b + c == '.^^':
                new_row += '^'
            elif a + b + c == '^..':
                new_row += '^'
            elif a + b + c == '..^':
                new_row += '^'
            else:
                new_row += '.'
                count += 1

        last = new_row

    return count


def main():
    with open(f'{dir_path}/../../inputs/day18/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

