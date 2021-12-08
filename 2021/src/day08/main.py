import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    count = 0
    for line in inp:
        for seg in line:
            if len(seg) in [2, 3, 4, 7]:
                count += 1
    return count


inters = {
    1: {
        0: 2,
        2: 1,
        3: 2,
        5: 1,
        6: 1,
        9: 2
    },
    0: {
        2: 4,
        3: 4,
        5: 4,
        6: 5,
        9: 5
    },
    2: {
        3: 4,
        5: 3,
        6: 4,
        9: 4
    },
    3: {
        5: 4,
        6: 4,
        9: 5
    },
    5: {
        6: 5,
        9: 5
    }
}


def decode(line):
    # XDDDD
    codes = {}
    revcodes = {}
    iline = line[:]

    for i, seg in enumerate(iline):
        if seg in codes:
            continue
        elif len(seg) == 2:
            codes[seg] = 1
            revcodes[1] = seg
            continue
        elif len(seg) == 3:
            codes[seg] = 7
            revcodes[7] = seg
            continue
        elif len(seg) == 4:
            codes[seg] = 4
            revcodes[4] = seg
            continue
        elif len(seg) == 7:
            codes[seg] = 8
            revcodes[8] = seg
            continue
        elif len(seg) == 5: #2, 3, 5
            if 2 in revcodes and 3 in revcodes:
                codes[seg] = 5
                revcodes[5] = seg
                continue
            elif 2 in revcodes and 5 in revcodes:
                codes[seg] = 3
                revcodes[3] = seg
                continue
            elif 3 in revcodes and 5 in revcodes:
                codes[seg] = 2
                revcodes[2] = seg
                continue

            if 1 in revcodes:
                if len(set(revcodes[1]).intersection(set(seg))) == 2:
                    codes[seg] = 3
                    revcodes[3] = seg
                    continue
            if 2 in revcodes:
                if len(set(revcodes[2]).intersection(set(seg))) == 3:
                    codes[seg] = 5
                    revcodes[5] = seg
                else:
                    codes[seg] = 3
                    revcodes[3] = seg
                continue
            if 4 in revcodes:
                if len(set(revcodes[4]).intersection(set(seg))) == 2:
                    codes[seg] = 2
                    revcodes[2] = seg
                    continue
            if 5 in revcodes:
                if len(set(revcodes[5]).intersection(set(seg))) == 3:
                    codes[seg] = 2
                    revcodes[2] = seg
                else:
                    codes[seg] = 3
                    revcodes[3] = seg
                continue
            if 6 in revcodes:
                if len(set(revcodes[6]).intersection(set(seg))) == 5:
                    codes[seg] = 5
                    revcodes[5] = seg
                    continue
            if 7 in revcodes:
                if len(set(revcodes[7]).intersection(set(seg))) == 3:
                    codes[seg] = 3
                    revcodes[3] = seg
                    continue
            if 9 in revcodes:
                if len(set(revcodes[9]).intersection(set(seg))) == 4:
                    codes[seg] = 2
                    revcodes[2] = seg
                    continue
        elif len(seg) == 6: #0, 6, 9
            if 0 in revcodes and 6 in revcodes:
                codes[seg] = 9
                revcodes[9] = seg
                continue
            elif 0 in revcodes and 9 in revcodes:
                codes[seg] = 6
                revcodes[6] = seg
                continue
            elif 6 in revcodes and 9 in revcodes:
                codes[seg] = 0
                revcodes[0] = seg
                continue

            if 1 in revcodes:
                if len(set(revcodes[1]).intersection(set(seg))) == 1:
                    codes[seg] = 6
                    revcodes[6] = seg
                    continue
            if 3 in revcodes:
                if len(set(revcodes[3]).intersection(set(seg))) == 5:
                    codes[seg] = 9
                    revcodes[9] = seg
                    continue
            if 4 in revcodes:
                if len(set(revcodes[4]).intersection(set(seg))) == 4:
                    codes[seg] = 9
                    revcodes[9] = seg
                    continue
            if 5 in revcodes:
                if len(set(revcodes[5]).intersection(set(seg))) == 4:
                    codes[seg] = 0
                    revcodes[0] = seg
                    continue
            if 7 in revcodes:
                if len(set(revcodes[7]).intersection(set(seg))) == 2:
                    codes[seg] = 6
                    revcodes[6] = seg
                    continue
            if 9 in revcodes:
                if len(set(revcodes[9]).intersection(set(seg))) == 4:
                    codes[seg] = 2
                    revcodes[2] = seg
                    continue

        iline.append(seg)
        if i == 1000:
            exit(1)

    return codes


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

