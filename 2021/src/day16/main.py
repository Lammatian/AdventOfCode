import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from functools import reduce


def parse_packet(inp, s):
    version = int(inp[s:s+3], 2)
    ptype = int(inp[s+3:s+6], 2)
    s += 6
    if ptype == 4:
        literal = ''
        while True:
            group = inp[s:s+5]
            literal += group[1:]
            s += 5
            if group[0] == '0':
                break
        return (s, version, ptype, int(literal, 2))
    else:
        i = inp[s]
        s += 1
        if i == '0':
            length = int(inp[s:s+15], 2)
            s += 15
            start = s
            packets = []
            while s - start < length:
                packet = parse_packet(inp, s)
                s = packet[0]
                packets.append(packet)
            return (s, version, ptype, packets)
        else:
            n_of_ps = int(inp[s:s+11], 2)
            s += 11
            packets = []
            for _ in range(n_of_ps):
                packet = parse_packet(inp, s)
                s = packet[0]
                packets.append(packet)
            return (s, version, ptype, packets)


def part1(inp):
    packets = [parse_packet(inp, 0)]
    version = 0
    
    while packets:
        packet = packets.pop(0)
        version += packet[1]

        if packet[2] == 4:
            continue
        else:
            packets += packet[3]

    return version


def eval_packet(packet):
    (s, version, ptype, sth) = packet

    if ptype == 4:
        return sth
    else:
        evald = map(eval_packet, sth)
        if ptype == 0:
            return sum(evald)
        elif ptype == 1:
            return reduce(lambda x, y: x*y, evald, 1)
        elif ptype == 2:
            return min(evald)
        elif ptype == 3:
            return max(evald)
        elif ptype == 5:
            first, second = list(evald)[:2]
            return 1 if first > second else 0
        elif ptype == 6:
            first, second = list(evald)[:2]
            return 1 if first < second else 0
        elif ptype == 7:
            first, second = list(evald)[:2]
            return 1 if first == second else 0


def part2(inp):
    return eval_packet(parse_packet(inp, 0))


def main():
    with open(f'{dir_path}/../../inputs/day16/input') as f:
        inp = f.read().strip()

    binp = bin(int(inp, 16))[2:].zfill(4 * len(inp))

    print(binp)
    
    print(part1(binp[:]))
    print(part2(binp[:]))


if __name__ == '__main__':
    main()

