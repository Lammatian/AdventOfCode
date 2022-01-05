import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    idx = 0
    while idx < len(inp):
        if inp[idx] == '(':
            end = inp.find(')', idx)
            l, t = list(map(int, inp[idx+1:end].split('x')))
            cont = end + l * t - (end - idx)
            inp = inp[:idx] + inp[end+1:end+l+1] * t + inp[end+l+1:]
            idx = cont
        else:
            idx += 1

    return len(inp)


def find_enclosed(substr):
    idx = 0
    result = []
    while True:
        start = substr.find('(')
        if start == -1:
            break 
        end = substr.find(')', start)
        l, t = list(map(int, substr[start+1:end].split('x')))
        result.append((l, t, start, end))
        idx = end + 1
    return result


def find_length(substr):
    result = 0
    idx = 0
    while True:
        start = substr.find('(', idx)
        if start == -1:
            break
        end = substr.find(')', start)
        result += start - idx
        l, t = list(map(int, substr[start+1:end].split('x')))
        idx = end + l + 1
        result += t * find_length(substr[end+1:end+l+1])
    result += len(substr) - idx
    return result


def part2(inp):
    return find_length(inp)


def main():
    with open(f'{dir_path}/../../inputs/day09/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))[0]

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

