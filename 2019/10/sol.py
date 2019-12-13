import sys
from fractions import Fraction
from functools import cmp_to_key

def gcd(a, b):
    while b != 0:
        a, b = b, a % b

    return a


class Coprimes:
    def __init__(self, x, y):
        if x == 0:
            self.x = 0
            self.y = 1 if y > 0 else -1
            return
        elif y == 0:
            self.x = 1 if x > 0 else -1
            self.y = 0
            return

        g = gcd(x, y)
        self.x = x // g
        self.y = y // g

    def __str__(self):
        return f'Coprimes ({self.x}, {self.y})'

    def __repr__(self):
        return self.__str__()

    def __hash__(self):
        return hash((self.x, self.y))
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y


def get_relative_positions(comets, x, y):
    positions = []

    for j, row in enumerate(comets):
        for i, c in enumerate(row):
            if c == '#' and (i, j) != (x, y):
                positions.append((i - x, j - y))

    return positions


def sol1(comets):
    best_pos = (0, 0)
    best_detected = 0
    count = 0

    for j, row in enumerate(comets):
        for i, c in enumerate(row):
            if c == '#':
                count += 1
                positions = get_relative_positions(comets, i, j)

                above = {Coprimes(x, y) for (x, y) in positions if y < 0}
                below = {Coprimes(x, y) for (x, y) in positions if y >= 0}
                visible = len(above) + len(below)
                
                if visible > best_detected:
                    best_detected = visible
                    best_pos = (i, j)


    return best_detected, best_pos


def get_quadrant(x, y):
    if x == 0:
        return 3 if y > 0 else 1
    elif y == 0:
        return 2 if x > 0 else 4

    if x * y > 0:
        return 2 if x > 0 else 4
    else:
        return 1 if x > 0 else 3


def get_tangens(x, y, q):
    if q == 1 or q == 3:
        return Fraction(x, -y)
    elif q == 2 or q == 4:
        return Fraction(y, x)
    else:
        return None


def angle_comparator(p1, p2):
    """
    Sorts by angle
    If same angle, sorts by distance from origin
    """
    x1, y1 = p1
    x2, y2 = p2 
    q1 = get_quadrant(x1, y1)
    q2 = get_quadrant(x2, y2)

    if q1 != q2:
        return 1 if q1 > q2 else -1

    tg1 = get_tangens(x1, y1, q1)
    tg2 = get_tangens(x2, y2, q2)

    if tg1 != tg2:
        return 1 if tg1 > tg2 else -1

    return 1 if abs(x1) > abs(x2) else -1


def sol2(comets, x, y):
    # get relative positions
    relative_positions = get_relative_positions(comets, x, y)

    # sort by angle starting from (0, y)
    relative_positions.sort(key=cmp_to_key(angle_comparator))
    # save absolute positions for solution recovery
    rel_abs_positions = [((i, j), (i + x, j + y)) for (i, j) in relative_positions]
    
    order = [[]]
    last_angle = None

    # Put comets into 'buckets' with same angle
    for rel_comet, abs_comet in rel_abs_positions:
        if last_angle and last_angle == Coprimes(*rel_comet):
            order[-1].append(abs_comet)
        else:
            order.append([abs_comet])

        last_angle = Coprimes(*rel_comet)

    count = 0
    cur = 0

    # Pop to count, not really necessary to pop
    # could keep index instead
    while count < 199:
        if len(order[cur]) > 0:
            popped = order[cur].pop(0)
            count += 1

        cur = (cur + 1) % len(order)

    return order[cur][0]


def main():
    with open(sys.argv[1]) as f:
        comets = f.readlines()

    _, best_point = sol1(comets)
    print(sol1(comets))
    print(sol2(comets, *best_point))


if __name__ == '__main__':
    main()