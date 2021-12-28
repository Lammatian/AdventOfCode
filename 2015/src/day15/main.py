import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(inp):
    best = 0
    best_dist = None
    for a in range(101):
        for b in range(101 - a):
            for c in range(101 - a - b):
                d = 100 - a - b - c
                score = 1
                l = [a, b, c, d]
                for prop in range(4):
                    score_ = 0
                    for ing in range(len(inp)):
                        score_ += l[ing] * inp[ing][1][prop]

                    score *= max(score_, 0)

                if score > best:
                    best = score
                    best_dist = (a, b, c, d)

    return best, best_dist


def part2(inp):
    best = 0
    best_dist = None
    for a in range(101):
        for b in range(101 - a):
            for c in range(101 - a - b):
                d = 100 - a - b - c
                score = 1
                l = [a, b, c, d]
                for prop in range(4):
                    score_ = 0
                    for ing in range(len(inp)):
                        score_ += l[ing] * inp[ing][1][prop]

                    score *= max(score_, 0)

                calories = sum(l[i] * inp[i][1][-1] for i in range(len(inp)))
                if score > best and calories == 500:
                    best = score
                    best_dist = (a, b, c, d)

    return best, best_dist


def main():
    with open(f'{dir_path}/../../inputs/day15/input') as f:
        inp = list(map(lambda x: [x.split(': ')[0], x.split(': ')[1].split(', ')], f.read().strip().split('\n')))

    print(inp)
    for i, ing in enumerate(inp):
        inp[i][1] = list(map(lambda x: int(x.split()[1]), inp[i][1]))
    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

