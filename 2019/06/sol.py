import sys
from collections import defaultdict

def sol1(orbs):
    orb_d = defaultdict(list)

    for orbiter, orbitee in orbs:
        orb_d[orbiter].append(orbitee)

    q = [('COM', 0)]
    result = 0

    while q:
        name, orbits = q.pop()
        result += orbits

        q.extend([(o, orbits + 1) for o in orb_d[name]])

    return result


def get_path_to_COM(source, orbiters):
    cur = orbiters[source]
    path = [cur]

    while cur != 'COM':
        cur = orbiters[cur]
        path.append(cur)

    return path[::-1]


def sol2(orbs):
    orbitees = defaultdict(list)
    orbiters = {}

    for orbiter, orbitee in orbs:
        orbitees[orbiter].append(orbitee)
        orbiters[orbitee] = orbiter

    santa_path = get_path_to_COM('SAN', orbiters)
    my_path = get_path_to_COM('YOU', orbiters)

    path_length = len(santa_path) + len(my_path)

    for a, b in zip(santa_path, my_path):
        if a != b:
            break

        path_length -= 2

    return path_length


def main():
    with open(sys.argv[1]) as f:
        orbits = list(map(lambda x: x.split(')'), f.read().splitlines()))

    print(orbits)

    print(sol1(orbits))
    print(sol2(orbits))


if __name__ == '__main__':
    main()