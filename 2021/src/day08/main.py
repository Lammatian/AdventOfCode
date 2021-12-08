import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    count = 0
    for line in inp:
        for seg in line:
            if len(seg) in [2, 3, 4, 7]:
                count += 1
    return count


def decode(line):
    p1 = next(seg for seg in line if len(seg) == 2)
    line.remove(p1)
    p7 = next(seg for seg in line if len(seg) == 3)
    line.remove(p7)
    p4 = next(seg for seg in line if len(seg) == 4)
    line.remove(p4)
    p8 = next(seg for seg in line if len(seg) == 7)
    line.remove(p8)
    p2 = next(seg for seg in line if len(set(p4).intersection(set(seg))) == 2)
    line.remove(p2)
    p5 = next(seg for seg in line if len(set(p2).intersection(set(seg))) == 3)
    line.remove(p5)
    p6 = next(seg for seg in line if len(set(p1).intersection(set(seg))) == 1)
    line.remove(p6)
    p9 = next(seg for seg in line if len(set(p4).intersection(set(seg))) == 4)
    line.remove(p9)
    p0 = next(seg for seg in line if len(set(p6).intersection(set(seg))) == 5)
    line.remove(p0)
    p3 = next(seg for seg in line if len(set(p6).intersection(set(seg))) == 4)

    return {
        p0: 0,
        p1: 1,
        p2: 2,
        p3: 3,
        p4: 4,
        p5: 5,
        p6: 6,
        p7: 7,
        p8: 8,
        p9: 9
    }


def part2(inp):
    result = 0
    for line in inp:
        codes = decode(line[:-4])
        value = 0

        for seg in line[-4:]:
            value *= 10
            value += codes[seg]

        result += value
    
    return result


def main():
    with open(f'{dir_path}/../../inputs/day08/input') as f:
        inp1 = list(map(lambda x: x.split(' | ')[1].split(), f.read().strip().split('\n')))
    
    with open(f'{dir_path}/../../inputs/day08/input') as f:
        inp2 = list(map(lambda x: x.replace('| ', '').split(), f.read().strip().split('\n')))
        inp2 = list(map(lambda x: [''.join(sorted(s)) for s in x], inp2))
    
    print(part1(inp1[:]))
    print(part2(inp2[:]))


if __name__ == '__main__':
    main()

