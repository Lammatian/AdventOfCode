import os
import json
dir_path = os.path.dirname(os.path.realpath(__file__))


def count_ints(js):
    result = 0
    if type(js) == list:
        for elem in js:
            if type(elem) == list:
                result += count_ints(elem)
            elif type(elem) == dict:
                result += count_ints(elem)
            elif type(elem) == int:
                result += elem
    if type(js) == dict:
        for _, v in js.items():
            if type(v) == list:
                result += count_ints(v)
            elif type(v) == dict:
                result += count_ints(v)
            elif type(v) == int:
                result += v
    if type(js) == int:
        result += js

    return result


def part1(inp):
    return count_ints(inp)
    

def count_nred(js):
    result = 0
    if type(js) == list:
        for elem in js:
            if type(elem) == list:
                result += count_nred(elem)
            elif type(elem) == dict:
                result += count_nred(elem)
            elif type(elem) == int:
                result += elem
    if type(js) == dict:
        if 'red' in js.keys() or 'red' in js.values():
            return 0

        for _, v in js.items():
            if type(v) == list:
                result += count_nred(v)
            elif type(v) == dict:
                result += count_nred(v)
            elif type(v) == int:
                result += v
    if type(js) == int:
        result += js

    return result


def part2(inp):
    return count_nred(inp)


def main():
    with open(f'{dir_path}/../../inputs/day12/input') as f:
        inp = json.load(f)

    print(inp)
    
    print(part1(inp))
    print(part2(inp))


if __name__ == '__main__':
    main()

