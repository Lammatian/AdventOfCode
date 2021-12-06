import os
from types import new_class
dir_path = os.path.dirname(os.path.realpath(__file__))


def next_pass(pwd):
    i = len(pwd) - 1
    while pwd[i] == 'z':
        pwd[i] = 'a'
        i -= 1
    
    pwd[i] = chr(ord(pwd[i]) + 1) 
    return pwd


def increasing(pwd):
    last = -1
    count = 0

    for c in pwd:
        if ord(c) - last == 1:
            count += 1
        else:
            count = 1

        last = ord(c)

        if count == 3:
            return True

    return False


def contains_iol(pwd):
    return len(set(pwd) - set("iol")) == len(set(pwd))


def consecutive(pwd):
    last = -1
    count = 0 
    consec = 0

    for c in pwd:
        if c == last:
            count += 1
        else:
            count = 1

        if count == 2:
            consec += 1
            count = 0

        if consec == 2:
            return True

        last = c

    return False


def meets_requirements(pwd):
    return increasing(pwd) and contains_iol(pwd) and consecutive(pwd) 


def part1(inp):
    while not meets_requirements(inp):
        inp = next_pass(inp)

    return ''.join(inp)


def part2(inp):
    while not meets_requirements(inp):
        inp = next_pass(inp)

    return ''.join(inp)


def main():
    with open(f'{dir_path}/../../inputs/day11/input') as f:
        inp = list(f.read().strip())
    
    print(part1(inp[:]))
    print(part2(next_pass(list(part1(inp[:])))))


if __name__ == '__main__':
    main()

