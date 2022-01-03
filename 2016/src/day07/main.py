import os
dir_path = os.path.dirname(os.path.realpath(__file__))

def has_abba(s):
    for a, b, c, d in zip(s, s[1:], s[2:], s[3:]):
        if a != b and a == d and b == c:
            return True
    return False


def split_ip(ip, closing):
    ipsplit = ip.split('[' if not closing else ']', 1)
    if len(ipsplit) == 1:
        return ipsplit

    return [ipsplit[0]] + split_ip(ipsplit[1], not closing)


def part1(inp):
    count = 0
    for line in inp:
        ipsplit = split_ip(line, False)
        in_brackets = False
        found_abba = False
        for split in ipsplit:
            if in_brackets and has_abba(split):
                break
            elif not in_brackets and has_abba(split):
                found_abba = True
            in_brackets = not in_brackets
        else:
            if found_abba:
                count += 1

    return count


def find_abas(ip, is_bab):
    abas = set()
    for a, b, c in zip(ip, ip[1:], ip[2:]):
        if a == c and a != b:
            if is_bab:
                abas.add((b, a))
            else:
                abas.add((a, b))

    return abas


def part2(inp):
    count = 0
    for line in inp:
        ipsplit = split_ip(line, False)
        abas = set()
        babs = set()
        in_brackets = False
        for split in ipsplit:
            if in_brackets:
                babs.update(find_abas(split, True))
            elif not in_brackets:
                abas.update(find_abas(split, False))
            in_brackets = not in_brackets

        if abas.intersection(babs):
            count += 1

    return count


def main():
    with open(f'{dir_path}/../../inputs/day07/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

