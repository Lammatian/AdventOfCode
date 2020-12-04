import sys
from copy import deepcopy
from queue import PriorityQueue, Queue


class Board:
    # ASSUMES EACH TWO PATHS TO THE SAME THING NEED SAME KEYS
    # (holds for my input lol)
    def __init__(self, board_str):
        self.board = board_str
        self.k2o = {}

        self.keys = {}
        self.start_position = None
        self.current_object = '@'
        self.reach = {}

        for y in range(len(board_str)):
            for x, point in enumerate(board_str[y]):
                if point == '@':
                    self.start_position = (x, y)
                elif point not in ['.', '#']:
                    if self._is_key((x, y)):
                        self.keys[point] = (x, y)
                        self.k2o[point] = {}

        self.collected = 0

        self._get_key_to_object_distances()
        self._get_reachable(self.start_position, deepcopy(board_str), set())

    def __gt__(self, board2):
        return len(self.collected) >= len(board2.collected)

    def _get_key_to_object_distances(self):
        for key, position in self.keys.items():
            self._get_distances_from_key(key, position)

    def _get_reachable(self, start_pos, visited, keys_needed):
        visited[start_pos[1]][start_pos[0]] = '#'

        for n_pos in Board._neighbours(*start_pos):
            n_x, n_y = n_pos
            n_obj = visited[n_y][n_x]

            if Board._is_object(visited, n_pos):
                if n_obj.isupper():
                    self._get_reachable(n_pos, visited, set(list(keys_needed) + [n_obj.lower()]))
                else:
                    self.reach[n_obj] = Board._k2k(keys_needed)
                    self._get_reachable(n_pos, visited, keys_needed)
            elif n_obj == '.':
                self._get_reachable(n_pos, visited, keys_needed)

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
        self.current_object = key

    def uncollect(self, key, prev_key):
        self.collected -= 1 << (ord(key) - ord('a')) 
        self.current_object = prev_key

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

    if (b.collected, b.current_object) in memo:
        return memo[(b.collected, b.current_object)]

    best_score = 1e10
    start_key = b.current_object

    for key in b.keys.keys():
        if not b.can_reach(key) or b.is_collected(key):
            continue

        b.collect(key)
        score = best_path(b, memo) + b.k2o[key][start_key]
        best_score = min(score, best_score)
        b.uncollect(key, start_key)

    memo[(b.collected, b.current_object)] = best_score
    return best_score


def sol1(data):
    board = Board(data)

    return best_path(board, {})


def sol2(data):
    pass


def main():
    with open(sys.argv[1]) as f:
        data = list(map(list, f.read().splitlines()))

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()