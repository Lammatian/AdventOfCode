import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    result = 0
    for row in inp:
        result += max(row) - min(row)
    return result 


def part2(inp):
    result = 0
    for row in inp:
        done = False
        for x in row:
            for y in row:
                if x % y == 0 and x != y:
                    result += x // y
                    done = True
                    break
            if done:
                break
    return result


def main():
    with open(f'{dir_path}/../../inputs/day02/input') as f:
        inp = list(map(lambda x: list(map(int, x.split())), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

