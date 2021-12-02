import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    signals = {}

    for lhs, rhs in inp:
        lhs_ = lhs.split()

        if len(lhs_) == 1:
            if lhs_[0].isdigit():
                signals[rhs] = int(lhs_[0])
            elif lhs_[0] in signals:
                signals[rhs] = signals[lhs_[0]]
            else:
                inp.append((lhs, rhs))
        elif len(lhs_) == 2:
            a = lhs_[1]
            if a.isdigit():
                signals[rhs] = int(a) ^ 65535
            elif a in signals:
                signals[rhs] = signals[a] ^ 65535
            else:
                inp.append((lhs, rhs))
        else:
            a, b = -1, -1
            if lhs_[0].isdigit():
                a = int(lhs_[0])
            elif lhs_[0] in signals:
                a = signals[lhs_[0]]
            if lhs_[2].isdigit():
                b = int(lhs_[2])
            elif lhs_[2] in signals:
                b = signals[lhs_[2]]
            if min(a, b) == -1:
                inp.append((lhs, rhs))
                continue

            if lhs_[1] == 'AND':
                signals[rhs] = a & b
            elif lhs_[1] == 'OR':
                signals[rhs] = a | b
            elif lhs_[1] == 'LSHIFT':
                signals[rhs] = a << b
            elif lhs_[1] == 'RSHIFT':
                signals[rhs] = a >> b

        if 'a' in signals:
            return signals['a']


def part2(inp):
    inp = [(lhs, rhs) for lhs, rhs in inp if rhs != 'b']
    inp = [('3176', 'b')] + inp

    signals = {}

    for lhs, rhs in inp:
        lhs_ = lhs.split()

        if len(lhs_) == 1:
            if lhs_[0].isdigit():
                signals[rhs] = int(lhs_[0])
            elif lhs_[0] in signals:
                signals[rhs] = signals[lhs_[0]]
            else:
                inp.append((lhs, rhs))
        elif len(lhs_) == 2:
            a = lhs_[1]
            if a.isdigit():
                signals[rhs] = int(a) ^ 65535
            elif a in signals:
                signals[rhs] = signals[a] ^ 65535
            else:
                inp.append((lhs, rhs))
        else:
            a, b = -1, -1
            if lhs_[0].isdigit():
                a = int(lhs_[0])
            elif lhs_[0] in signals:
                a = signals[lhs_[0]]
            if lhs_[2].isdigit():
                b = int(lhs_[2])
            elif lhs_[2] in signals:
                b = signals[lhs_[2]]
            if min(a, b) == -1:
                inp.append((lhs, rhs))
                continue

            if lhs_[1] == 'AND':
                signals[rhs] = a & b
            elif lhs_[1] == 'OR':
                signals[rhs] = a | b
            elif lhs_[1] == 'LSHIFT':
                signals[rhs] = a << b
            elif lhs_[1] == 'RSHIFT':
                signals[rhs] = a >> b

        if 'a' in signals:
            return signals['a']


def main():
    with open(f'{dir_path}/../../inputs/day07/input') as f:
        inp = list(map(lambda x: (x.split(' -> ')[0], x.split(' -> ')[1]), f.read().strip().split('\n')))
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

