import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def part1(inp, the_aunt):
    for aunt, stock in inp:
        for k, v in the_aunt.items():
            if k in stock and stock[k] != v:
                break
        else:
            return int(aunt.split()[1])


def part2(inp, the_aunt):
    for aunt, stock in inp:
        for k, v in the_aunt.items():
            if k in stock:
                if k in ['cats', 'trees'] and stock[k] <= v:
                    break
                elif k in ['pomeranians', 'goldfish'] and stock[k] >= v:
                    break
                elif k not in ['cats', 'trees', 'pomeranians', 'goldfish'] and stock[k] != v:
                    break
        else:
            return int(aunt.split()[1])


def main():
    with open(f'{dir_path}/../../inputs/day16/input') as f:
        inp = list(map(lambda x: x.split(': ', 1), f.read().strip().split('\n')))

    for i, aunt in enumerate(inp):
        aunt[1] = aunt[1].split(', ')
        inp[i][1] = {k: int(v) for k, v in list(map(lambda x: x.split(': '), aunt[1]))}

    the_aunt = {
        'children': 3,
        'cats': 7,
        'samoyeds': 2,
        'pomeranians': 3,
        'akitas': 0,
        'vizslas': 0,
        'goldfish': 5,
        'trees': 3,
        'cars': 2,
        'perfumes': 1
    }
        
    print(inp)
    
    print(part1(inp[:], the_aunt))
    print(part2(inp[:], the_aunt))


if __name__ == '__main__':
    main()

