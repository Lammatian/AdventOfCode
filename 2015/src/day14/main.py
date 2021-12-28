import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict


def distance(v, t, rest, seconds):
    d = (v * t) * (seconds // (t + rest))
    d += v * min(t, seconds % (t + rest))
    return d


def part1(inp):
    best = 0 
    for _, v, t, rest in inp:
        best = max(best, distance(v, t, rest, 2503))

    return best


def run_second(second, reindeers, distances, scores):
    best_distance = 0
    best_reindeers = []
    for name, v, t, rest in reindeers:
        if second % (t + rest) < t:
            distances[name] += v

        if distances[name] > best_distance:
            best_distance = distances[name]
            best_reindeers = [name]
        elif distances[name] == best_distance:
            best_reindeers.append(name)

    for r in best_reindeers:
        scores[r] += 1


def part2(inp):
    distances = defaultdict(int)
    scores = defaultdict(int)
    for second in range(2504):
        run_second(second, inp, distances, scores)

    return max(v for _, v in scores.items())


def main():
    with open(f'{dir_path}/../../inputs/day14/input') as f:
        inp = list(map(lambda x: (x.split()[0], int(x.split()[3]), int(x.split()[6]), int(x.split()[-2])), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

