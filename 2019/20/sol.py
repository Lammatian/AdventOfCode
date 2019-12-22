import sys
from collections import defaultdict
from queue import Queue
from copy import deepcopy

def print_board(board, width=1):
    fmt = str(r'{: ^' + str(width) + r'}')
    board_str = '\n'.join(map(lambda x: ''.join(map(lambda s: fmt.format(s), x)), board))

    print(board_str)


def in_range(x, y, board):
    return 0 <= y and y < len(board) and 0 <= x and x < len(board[0])


def neighbours(board, x, y):
    """
    Return all valid neighbouring places on the board as a
    generator
    """
    for a, b in [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]:
        if in_range(a, b, board):
            yield (a, b)


def find_all_portals(board):
    portals = defaultdict(list) # name: [(x1, y1), (x2, y2)]

    for y, row in enumerate(board):
        for x, b in enumerate(row):
            if board[y][x] == '.':
                for xn, yn in neighbours(board, x, y):
                    if str.isalpha(board[yn][xn]):
                        x_, y_ = 2*xn - x, 2*yn - y
                        max_x = max(xn, x_)
                        min_x = min(xn, x_)
                        max_y = max(yn, y_)
                        min_y = min(yn, y_)
                        name = board[min_y][min_x] + board[max_y][max_x]

                        portals[name].append((x, y))

    return {name: coords for name, coords in portals.items() if name in ['AA', 'ZZ'] or len(coords) == 2}


def portal_other_side(points, x, y, border_xs=None, border_ys=None):
    if not border_xs:
        border_xs = []

    if not border_ys:
        border_ys = []

    level_change = -1 if x in border_xs or y in border_ys else 1

    if (x, y) == points[0]:
        return points[1], level_change
    else:
        return points[0], level_change


def sol1(board, debug=False):
    portals = find_all_portals(board)
    portals_rev = {}

    for name, points in portals.items():
        for p in points:
            portals_rev[p] = name

    visited_portals = set(['AA'])
    entrance = portals['AA'][0]

    q = Queue()
    q.put((entrance, 0)) # (point, distance)
    board[entrance[1]][entrance[0]] = str(0)

    while not q.empty():
        point, d = q.get()

        for x, y in neighbours(board, *point):
            if board[y][x] == '.':
                board[y][x] = str(d + 1)

                if (x, y) in portals_rev: # is entrance
                    portal_name = portals_rev[(x, y)]

                    if portal_name == 'ZZ':
                        return d + 1

                    other_side, _ = portal_other_side(portals[portal_name], x, y)
                    visited_portals.add(portals_rev[(x, y)])
                    q.put((other_side, d + 2))
                    board[other_side[1]][other_side[0]] = str(d + 2)

                    if debug:
                        print_board(board, 3)
                else:
                    q.put(((x, y), d + 1))


def portal_level_change(x, y, border_xs, border_ys):
    return -1 if x in border_xs or y in border_ys else 1


def find_all_reachable(board, x, y, portals, border_xs, border_ys):
    """
    Given a starting spot (portal), find all the portals
    reachable from that portal and return their coordinates
    and the number of steps required to reach it and the
    level change when entering that portal

    (BFS)
    """
    q = Queue()
    visited = set([(x, y)])
    board[y][x] = str(0)
    q.put(((x, y), 0))
    result = []
    portal_entrances = set()

    for name, ps in portals.items():
        portal_entrances.update(ps)

    while not q.empty():
        point, d = q.get()

        for xn, yn in neighbours(board, *point):
            if board[yn][xn] == '.':
                board[yn][xn] = str(d + 1)
                if (xn, yn) in portal_entrances:
                    level_change = portal_level_change(xn, yn, border_xs, border_ys)
                    result.append(((xn, yn), d + 1, level_change))
                else:
                    q.put(((xn, yn), d + 1))

    return result


def find_all_paths(portals, board, border_xs, border_ys):
    """
    Given all portals, find all the possible paths from
    each portal along with the length and the level
    change
    """
    paths = {}

    for name, ps in portals.items():
        for p in ps:
            paths[p] = find_all_reachable(deepcopy(board), *p, portals, border_xs, border_ys)

    return paths


def sol2(board, debug=False):
    # TODO: Using `find_all_paths`, make it so that you
    # don't have to re-search the same path on different
    # levels, but just use the pre-computed lengths
    portals = find_all_portals(board)
    portals_rev = {}

    border_xs = [2, len(board[0]) - 4]
    border_ys = [2, len(board) - 3]

    for name, points in portals.items():
        for p in points:
            portals_rev[p] = name

    boards = {0: deepcopy(board)}
    cur_level = 0
    entrance = portals['AA'][0]

    q = Queue()
    q.put((entrance, 0, 0, [('AA', 0)])) # (point, level, distance, name)
    boards[cur_level][entrance[1]][entrance[0]] = str(0)

    while not q.empty():
        point, l, d, names = q.get()

        for x, y in neighbours(board, *point):
            if boards[l][y][x] == '.':
                boards[l][y][x] = str(d + 1)

                if (x, y) in portals_rev: # is entrance
                    portal_name = portals_rev[(x, y)]

                    if portal_name == 'ZZ' and l == 0:
                        if debug:
                            print('Final path:', names + [(portal_name, l + lc)])

                        return d + 1
                    elif portal_name in ['AA', 'ZZ']:
                        continue

                    other_side, lc = portal_other_side(portals[portal_name], x, y, border_xs, border_ys)

                    if l + lc < 0:
                        continue

                    q.put((other_side, l + lc, d + 2, names + [(portal_name, l + lc)]))

                    if l + lc not in boards:
                        boards[l + lc] = deepcopy(board)

                    boards[l + lc][other_side[1]][other_side[0]] = str(d + 2)

                    if debug:
                        print_board(board, 3)
                else:
                    q.put(((x, y), l, d + 1, names))


def main():
    with open(sys.argv[1]) as f:
        data = list(map(list, f.readlines()))

    print(sol1(deepcopy(data)))
    print(sol2(deepcopy(data)))


if __name__ == '__main__':
    main()