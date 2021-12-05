import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def part1(inp):
    s = defaultdict(int)
    count = 0

    for xs, ys, xe, ye in inp:
        if xs == xe:
            for y in range(min(ys, ye), max(ys, ye) + 1):
                s[(xs, y)] += 1
                if s[(xs, y)] == 2:
                    count += 1
        elif ys == ye:
            for x in range(min(xs, xe), max(xs, xe) + 1):
                s[(x, ys)] += 1
                if s[(x, ys)] == 2:
                    count += 1
    
    return count

            


def part2(inp):
    s = defaultdict(int)
    count = 0

    for xs, ys, xe, ye in inp:
        if xs == xe:
            for y in range(min(ys, ye), max(ys, ye) + 1):
                s[(xs, y)] += 1
                if s[(xs, y)] == 2:
                    count += 1
        elif ys == ye:
            for x in range(min(xs, xe), max(xs, xe) + 1):
                s[(x, ys)] += 1
                if s[(x, ys)] == 2:
                    count += 1
        else:
            if xs < xe and ys < ye:
                for i in range(xe - xs + 1):
                    s[(xs + i, ys + i)] += 1
                    if s[(xs + i, ys + i)] == 2:
                        count += 1
            elif xs < xe:
                for i in range(xe - xs + 1):
                    s[(xs + i, ys - i)] += 1
                    if s[(xs + i, ys - i)] == 2:
                        count += 1
            elif ys < ye:
                for i in range(ye - ys + 1):
                    s[(xs - i, ys + i)] += 1
                    if s[(xs - i, ys + i)] == 2:
                        count += 1
            else:
                for i in range(xs - xe + 1):
                    s[(xs - i, ys - i)] += 1
                    if s[(xs - i, ys - i)] == 2:
                        count += 1
    
    return count


def main():
    with open(f'{dir_path}/../../inputs/day05/input') as f:
        inp = list(map(lambda x: x.split(' -> '), f.read().strip().split('\n')))
    
    instructions = []

    for row in inp:
        s = row[0].split(',')
        e = row[1].split(',')
        instructions.append((int(s[0]), int(s[1]), int(e[0]), int(e[1])))

    print(part1(instructions))
    print(part2(instructions))


if __name__ == '__main__':
    main()

