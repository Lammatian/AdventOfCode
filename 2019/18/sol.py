import sys
from copy import deepcopy
from queue import PriorityQueue, Queue
from typing import List


class Board:
    # ASSUMES EACH TWO PATHS TO THE SAME THING NEED SAME KEYS
    # (holds for my input lol)
    def __init__(self, board_str: List[str], divided: bool = False):
        # String representation of a board
        self.board = board_str
        # Distances from keys to other keys
        self.k2o = {}
        # Quadrants of keys
        self.k2q = {}
        self.divided = divided

        # Map from key to it's coordinates
        self.keys = {}
        # 0 is top-right quadrant, then clockwise
        self.quadrants = [0] if not divided else [0, 1, 2, 3]
        # Start position for each quadrant if applicable
        self.start_positions = {}
        self.current_objects = {q: '@' for q in self.quadrants}
        self.reach = {}

        for y in range(len(board_str)):
            for x, point in enumerate(board_str[y]):
                if point == '@':
                    self.start_positions[self._quadrant(x, y)] = (x, y)
                elif point not in ['.', '#']:
                    if self._is_key((x, y)):
                        self.keys[point] = (x, y)
                        self.k2o[point] = {}

        self.collected = 0

        self._get_key_to_object_distances()

        for quad, start_pos in self.start_positions.items():
            self._get_reachable(start_pos, deepcopy(board_str), set(), quad)

    def __gt__(self, board2):
        return len(self.collected) >= len(board2.collected)

    def _quadrant(self, x, y):
        if self.divided:
            # lines W//2 and H//2 is where the border between quadrants is for
            # the starting points
            H = len(self.board)
            W = len(self.board[0])

            if x > W//2 and y < H//2:
                return 0
            elif x > W//2 and y > H//2:
                return 1
            elif x < W//2 and y > H//2:
                return 2
            else:
                return 3
        else:
            # Just one "quadrant"
            return 0

    def _get_key_to_object_distances(self):
        for key, position in self.keys.items():
            self._get_distances_from_key(key, position)

    def _get_reachable(self, start_pos, visited, keys_needed, quadrant):
        visited[start_pos[1]][start_pos[0]] = '#'

        for n_pos in Board._neighbours(*start_pos):
            n_x, n_y = n_pos
            n_obj = visited[n_y][n_x]

            if Board._is_object(visited, n_pos):
                if n_obj.isupper():
                    self._get_reachable(n_pos, visited, set(list(keys_needed) + [n_obj.lower()]), quadrant)
                else:
                    self.reach[n_obj] = Board._k2k(keys_needed)
                    self.k2q[n_obj] = quadrant
                    self._get_reachable(n_pos, visited, keys_needed, quadrant)
            elif n_obj == '.':
                self._get_reachable(n_pos, visited, keys_needed, quadrant)
            
    def print_board(self):
        print('\n'.join(map(lambda x: ''.join(x), self.board)))

    # shortest distance from key to all objects (even if unreachable)
    def _get_distances_from_key(self, key, key_position):
        visited = deepcopy(self.board)
        visited[key_position[1]][key_position[0]] = '#'
        pos_dist = [(key_position, 0)]

        # BFS
        while pos_dist:
            cur_pos, cur_dist = pos_dist.pop(0)
            x, y = cur_pos

            for neighbour_pos in Board._neighbours(*cur_pos):
                n_x, n_y = neighbour_pos

                if self._is_key(neighbour_pos):
                    obj = visited[n_y][n_x]
                    self.k2o[key][obj] = cur_dist + 1
                
                if visited[n_y][n_x] != '#':
                    visited[n_y][n_x] = '#'
                    pos_dist.append((neighbour_pos, cur_dist + 1))

    def can_reach(self, key):
        # keys needed to reach key are a subset of collected
        return self.reach[key] | self.collected == self.collected

    def is_collected(self, key):
        return (1 << (ord(key) - ord('a'))) & self.collected == (1 << (ord(key) - ord('a')))
        
    def all_collected(self):
        return self.collected + 1 == (1 << len(self.keys))

    def collect(self, key):
        self.collected += 1 << (ord(key) - ord('a'))
        quadrant = self.k2q[key]
        self.current_objects[quadrant] = key

    def uncollect(self, key, prev_key):
        self.collected -= 1 << (ord(key) - ord('a'))
        quadrant = self.k2q[key]
        self.current_objects[quadrant] = prev_key

    @staticmethod
    def _k2k(keys):
        result = 0

        for key in keys:
            result += 1 << (ord(key) - ord('a'))

        return result

    def _is_key(self, position):
        elem = self.board[position[1]][position[0]]
        return elem.islower() or elem == '@'
    
    @staticmethod
    def _is_object(board, position):
        return board[position[1]][position[0]] not in ['.', '#']

    @staticmethod
    def _neighbours(x, y):
        return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


def best_path(b, memo):
    if b.all_collected():
        return 0

    if (b.collected, b.current_objects[0]) in memo:
        return memo[(b.collected, b.current_objects[0])]

    best_score = 1e10
    start_key = b.current_objects[0]

    for key in b.keys.keys():
        if not b.can_reach(key) or b.is_collected(key):
            continue

        b.collect(key)
        score = best_path(b, memo) + b.k2o[key][start_key]
        best_score = min(score, best_score)
        b.uncollect(key, start_key)

    memo[(b.collected, b.current_objects[0])] = best_score
    return best_score


def sol1(data):
    board = Board(data)
    return best_path(board, {})


def best_path_split(b, memo):
    if b.all_collected():
        return 0

    if (b.collected, tuple(sorted(b.current_objects.values()))) in memo:
        return memo[(b.collected, tuple(sorted(b.current_objects.values())))]

    best_score = 1e10
    start_keys = b.current_objects

    for key in b.keys.keys():
        if not b.can_reach(key) or b.is_collected(key):
            continue

        quadrant = b.k2q[key]
        prev_key = start_keys[quadrant]
        b.collect(key)
        score = best_path_split(b, memo) + b.k2o[key][prev_key]
        best_score = min(score, best_score)
        b.uncollect(key, prev_key)

    memo[(b.collected, tuple(sorted(b.current_objects.values())))] = best_score
    return best_score


def sol2(data):
    H = len(data)
    W = len(data[0])

    for x in range(-1, 2):
        for y in range(-1, 2):
            if x * y == 0:
                data[H//2 + y][W//2 + x] = '#'
            else:
                data[H//2 + y][W//2 + x] = '@'

    board = Board(data, divided=True)

    return best_path_split(board, {})


def main():
    with open(sys.argv[1]) as f:
        data = list(map(list, f.read().splitlines()))

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()