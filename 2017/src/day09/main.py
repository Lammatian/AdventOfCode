import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    new_inp = inp
    ignore_next = False
    while '!' in new_inp:
        inp = new_inp
        new_inp = ''
        for c in inp:
            if ignore_next:
                ignore_next = False
                continue
            if c == '!':
                ignore_next = True
                continue
            new_inp += c

    inp = new_inp
    new_inp = ''
    in_garbage = False
    for c in inp:
        if in_garbage:
            if c == '>':
                in_garbage = False
                continue
        else:
            if c == '<':
                in_garbage = True
                continue
            new_inp += c

    new_inp = new_inp.replace(',','')
    def sum_groups(clean):
        level = 0
        result = 0
        for c in clean:
            if c == '{':
                level += 1
            elif c == '}':
                result += level
                level -= 1
        return result
    return sum_groups(new_inp)


def part2(inp):
    new_inp = inp
    ignore_next = False
    while '!' in new_inp:
        inp = new_inp
        new_inp = ''
        for c in inp:
            if ignore_next:
                ignore_next = False
                continue
            if c == '!':
                ignore_next = True
                continue
            new_inp += c

    pre_garbage = new_inp
    inp = new_inp
    new_inp = ''
    in_garbage = False
    garbages = 0
    for c in inp:
        if in_garbage:
            if c == '>':
                garbages += 1
                in_garbage = False
                continue
        else:
            if c == '<':
                in_garbage = True
                continue
            new_inp += c

    return len(pre_garbage) - len(new_inp) - 2 * garbages


def main():
    with open(f'{dir_path}/../../inputs/day09/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)

    print(part1(inp[0]))
    print(part2(inp[0]))


if __name__ == '__main__':
    main()

