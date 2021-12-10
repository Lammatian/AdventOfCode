import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


CL = {
    '(': ')',
    '[': ']',
    '{': '}',
    '<': '>'
}


def part1(inp):
    global CL
    wrong = defaultdict(int)
    for line in inp:
        s = []

        for c in line:
            if c in ['(', '<', '[', '{']:
                s.append(c)
            elif c in [')', '>', '}', ']']:
                if s and CL[s[-1]] == c:
                    s.pop()
                else:
                    wrong[c] += 1
                    break

    result = 0

    for k, v in wrong.items():
        if k == ')':
            result += v * 3
        elif k == ']':
            result += v * 57
        elif k == '}':
            result += v * 1197
        elif k == '>':
            result += v * 25137

    return result


def part2(inp):
    global CL
    incomplete = []

    for line in inp:
        s = []

        for c in line:
            if c in ['(', '<', '[', '{']:
                s.append(c)
            elif c in [')', '>', '}', ']']:
                if s and CL[s[-1]] == c:
                    s.pop()
                else:
                    break
        else:
            incomplete.append(s[::-1])
                    
    results = []

    scores = {
        '(': 1,
        '[': 2,
        '{': 3,
        '<': 4
    }

    for s in incomplete:
        score = 0
        for c in s:
            score *= 5 
            score += scores[c]

        results.append(score)

    results.sort()

    return results[len(results) // 2]


def main():
    with open(f'{dir_path}/../../inputs/day10/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

