import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def string_code(val):
    count = 0
    escaping = False
    for c in val:
        if escaping:
            if c == 'x':
                count -= 1
            else:
                count += 1
            escaping = False
        else:
            if c == '\\':
                escaping = True
            else:
                count += 1

    return count - 2


def part1(inp):
    result = 0
    for val in inp:
        result += len(val) - string_code(val)

    return result


def encoding(val):
    result = 0
    for c in val:
        if c in ['\\', '"']:
            result += 2
        else:
            result += 1

    return result + 2


def part2(inp):
    result = 0
    for val in inp:
        result += encoding(val) - len(val)
        
    return result


def main():
    with open(f'{dir_path}/../../inputs/day08/input') as f:
        inp = f.read().strip().split('\n')
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

