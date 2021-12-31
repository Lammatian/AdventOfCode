import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import combinations

SHOP = """Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3"""

def get_rings(r):
    yield [0, 0, 0]

    for ring in r:
        yield ring

    for (cr1, dr1, ar1), (cr2, dr2, ar2) in combinations(r, 2):
        yield [cr1 + cr2, dr1 + dr2, ar1 + ar2]

def simulate(p, b):
    if p[1] <= b[2]:
        return False
    if b[1] <= p[2]:
        return True

    p_damage = p[1] - b[2]
    p_turns_to_win = (b[0] + p_damage - 1) // p_damage
    b_damage = b[1] - p[2]
    b_turns_to_win = (p[0] + b_damage - 1) // b_damage
    return p_turns_to_win <= b_turns_to_win

def part1(p, b, w, a, r):
    best_cost = 1e9
    for weapon_cost, weapon_damage, _ in w:
        for armor_cost, _, armor_armor in a + [[0, 0, 0]]:
            for ring_cost, ring_damage, ring_armor in get_rings(r):
                cost = weapon_cost + armor_cost + ring_cost
                damage = weapon_damage + ring_damage
                armor = armor_armor + ring_armor
                if simulate([100, damage, armor], b[:]) and cost < best_cost:
                    best_cost = cost

    return best_cost


def part2(p, b, w, a, r):
    best_cost = 0
    for weapon_cost, weapon_damage, _ in w:
        for armor_cost, _, armor_armor in a + [[0, 0, 0]]:
            for ring_cost, ring_damage, ring_armor in get_rings(r):
                cost = weapon_cost + armor_cost + ring_cost
                damage = weapon_damage + ring_damage
                armor = armor_armor + ring_armor
                if not simulate([100, damage, armor], b[:]) and cost > best_cost:
                    best_cost = cost

    return best_cost


def main():
    with open(f'{dir_path}/../../inputs/day21/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n')))

    player = [100, 0, 0]
    boss = list(map(lambda x: int(x.split()[-1]), inp))
    shop = list(map(lambda x: x.split('\n')[1:], SHOP.split('\n\n')))
    weapons = [list(map(int, x.split()[-3:])) for x in shop[0]]
    armor = [list(map(int, x.split()[-3:])) for x in shop[1]]
    rings = [list(map(int, x.split()[-3:])) for x in shop[2]]

    print(weapons)
    print(armor)
    print(rings)
    print(player, boss)
    
    print(part1(player[:], boss[:], weapons[:], armor[:], rings[:]))
    print(part2(player[:], boss[:], weapons[:], armor[:], rings[:]))


if __name__ == '__main__':
    main()

