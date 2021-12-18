import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from math import ceil, floor
from copy import deepcopy


class Pair():
    def __init__(self, left, right, parent):
        assert(not left or type(left) in [Pair, Num])
        assert(not right or type(right) in [Pair, Num])
        self.left = left
        self.right = right
        self.parent = parent

    def __repr__(self):
        return f'[{self.left}, {self.right}]'

    def __add__(self, other):
        p = Pair(self, other, None)
        self.parent = p
        other.parent = p
        return p

    def prev(self):
        if self.parent and self == self.parent.left:
            return self.parent.prev()
        elif self.parent and self == self.parent.right:
            return self.parent.left.rightmost()
        else:
            return None

    def nxt(self):
        if self.parent and self == self.parent.left:
            return self.parent.right.leftmost()
        elif self.parent and self == self.parent.right:
            return self.parent.nxt()
        else:
            return None

    def rightmost(self):
        return self.right.rightmost()

    def leftmost(self):
        return self.left.leftmost()

    def is_left(self):
        return self.parent and self == self.parent.left

    def is_right(self):
        return self.parent and self == self.parent.right

class Num():
    def __init__(self, value, parent):
        self.value = value
        self.parent = parent

    def __repr__(self):
        return f'{self.value}'

    def leftmost(self):
        return self

    def rightmost(self):
        return self

    def is_left(self):
        return self.parent and self == self.parent.left

    def is_right(self):
        return self.parent and self == self.parent.right


def treeify(snumber, parent=None):
    if type(snumber) == list:
        p = Pair(None, None, parent)
        p.left = treeify(snumber[0], p)
        p.right = treeify(snumber[1], p)
        return p
    else:
        return Num(snumber, parent)


def parse_line(line):
    return treeify(eval(line))


def str_snumber(snumber):
    return str(snumber)


def explode(snumber, depth):
    if type(snumber) == Num:
        return False

    if depth == 5:
        assert(type(snumber.left) == Num)
        assert(type(snumber.right) == Num)
        prev = snumber.prev()
        nxt = snumber.nxt()
        if prev:
            prev.value += snumber.left.value
        if nxt:
            nxt.value += snumber.right.value

        is_left = snumber.is_left()
        snumber = Num(0, snumber.parent)
        if is_left:
            snumber.parent.left = snumber
        else:
            snumber.parent.right = snumber

        return True
    else:
        if not explode(snumber.left, depth + 1):
            return explode(snumber.right, depth + 1)
        else:
            return True


def split(snumber):
    if type(snumber) == Num:
        val = snumber.value
        if val >= 10:
            if val % 2 == 0:
                lval, rval = val // 2, val // 2
            else:
                lval = val // 2
                rval = val // 2 + 1
                
            is_left = snumber.is_left()
            lnum = Num(lval, None)
            rnum = Num(rval, None)
            snumber = Pair(lnum, rnum, snumber.parent)
            snumber.left.parent = snumber
            snumber.right.parent = snumber
            if is_left:
                snumber.parent.left = snumber
            else:
                snumber.parent.right = snumber

            return True
        else:
            return False
    else:
        if not split(snumber.left):
            return split(snumber.right)
        else:
            return True


def magnitude(number):
    if type(number) == Num:
        return number.value
    else:
        return 3 * magnitude(number.left) + 2 * magnitude(number.right)


def reduce_once(cur):
    exploded = explode(cur, 1)
    while explode(cur, 1):
        pass

    splitted = split(cur)
    return exploded or splitted


def reduce(cur):
    while reduce_once(cur):
        pass
            

def part1(inp):
    cur = inp[0]
    for snum in inp[1:]:
        cur += snum
        reduce(cur)

    return magnitude(cur)


def part2(inp):
    max_mag = 0
    for i in range(len(inp)):
        for j in range(len(inp)):
            if i != j:
                snum = deepcopy(inp[i] + inp[j])
                reduce(snum)
                mag = magnitude(snum)
                max_mag = max(max_mag, magnitude(snum))

    return max_mag


def main():
    with open(f'{dir_path}/../../inputs/day18/input') as f:
        lines = f.read().strip().split('\n')
        print('\n'.join(lines))
        inp = list(map(parse_line, lines))

    print(inp)
    
    print(part1(deepcopy(inp)))
    print(part2(deepcopy(inp)))


if __name__ == '__main__':
    main()

