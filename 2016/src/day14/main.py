import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from hashlib import md5


def contains_conseq(s, l):
    last = ''
    count = 0
    for c in s:
        if c == last:
            count += 1
            if count >= l:
                return c
        else:
            count = 1
            last = c

    return None


def part1(inp):
    i = 0
    count = 0
    while True:
        h = md5((inp + str(i)).encode()).hexdigest()
        c = contains_conseq(h, 3)
        if c:
            for j in range(i + 1, i + 1001):
                h = md5((inp + str(j)).encode()).hexdigest()
                if 5 * c in h:
                    count +=1

                    if count == 64:
                        return i

                    break

        i += 1


def md5_(s):
    for _ in range(2017):
        s = md5(s.encode()).hexdigest()

    return s


def part2(inp):
    i = 0
    count = 0
    hashes = []
    for i in range(40000):
        hashes.append(md5_(inp + str(i)))

    for i, h in enumerate(hashes):
        c = contains_conseq(h, 3)
        if c:
            if any(5 * c in h2 for h2 in hashes[i + 1: i + 1001]):
                count += 1

                if count == 64:
                    return i

        i += 1


def main():
    with open(f'{dir_path}/../../inputs/day14/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

