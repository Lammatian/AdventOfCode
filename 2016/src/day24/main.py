import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import permutations, combinations


def find_shortest_path(board, start, end):
    R = len(board)
    C = len(board[0])
    queue = [(start, 0)]
    seen = set([start])

    while queue:
        (r, c), steps = queue.pop(0)

        DR = [-1, 1, 0, 0]
        DC = [0, 0, -1, 1]
        for dr, dc in zip(DR, DC):
            r_ = r + dr
            c_ = c + dc

            if 0 <= r_ < R and 0 <= c_ < C and (r_, c_) not in seen:
                if (r_, c_) == end:
                    return steps + 1
                elif board[r_][c_] == '.':
                    queue.append(((r_, c_), steps + 1))
                    seen.add((r_, c_))


def part1(inp):
    max_loc = 0
    coords = {}
    for r, row in enumerate(inp):
        for c, val in enumerate(row):
            if val not in '#.':
                coords[int(val)] = (r, c)
                max_loc = max(max_loc, int(val))

    shortest = {}
    for start, end in combinations(range(0, max_loc + 1), 2):
        start_c = coords[start]
        end_c = coords[end]
        path_length = find_shortest_path(inp, start_c, end_c)
        shortest[(start, end)] = path_length
        shortest[(end, start)] = path_length

    best = 1e9
    for perm in permutations(range(1, max_loc + 1)):
        perm = tuple([0] + list(perm))
        length = 0
        for a, b in zip(perm, perm[1:]):
            length += shortest[(a, b)]
        best = min(best, length)
            
    return best


def part2(inp):
    max_loc = 0
    coords = {}
    for r, row in enumerate(inp):
        for c, val in enumerate(row):
            if val not in '#.':
                coords[int(val)] = (r, c)
                max_loc = max(max_loc, int(val))

    shortest = {}
    for start, end in combinations(range(0, max_loc + 1), 2):
        start_c = coords[start]
        end_c = coords[end]
        path_length = find_shortest_path(inp, start_c, end_c)
        shortest[(start, end)] = path_length
        shortest[(end, start)] = path_length

    best = 1e9
    for perm in permutations(range(1, max_loc + 1)):
        perm = tuple([0] + list(perm) + [0])
        length = 0
        for a, b in zip(perm, perm[1:]):
            length += shortest[(a, b)]
        best = min(best, length)
            
    return best


def main():
    with open(f'{dir_path}/../../inputs/day24/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

