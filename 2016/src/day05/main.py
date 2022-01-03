import os
dir_path = os.path.dirname(os.path.realpath(__file__))
import hashlib


def part1(inp):
    i = 1
    count = 0
    result = ''
    while count < 8:
        to_hash = inp + str(i)
        hexhash = hashlib.md5(to_hash.encode()).hexdigest()
        if hexhash.startswith('00000'):
            result += hexhash[5]
            count += 1
        i += 1

    return result


def part2(inp):
    i = 1
    count = 0
    result = [' '] * 8
    seenpos = set()
    while count < 8:
        to_hash = inp + str(i)
        hexhash = hashlib.md5(to_hash.encode()).hexdigest()
        if hexhash.startswith('00000') and hexhash[5] in '01234567':
            pos = int(hexhash[5])
            if pos in seenpos:
                i += 1
                continue

            result[pos] = hexhash[6]
            seenpos.add(pos)
            count += 1

        i += 1

    return ''.join(result)


def main():
    with open(f'{dir_path}/../../inputs/day05/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

