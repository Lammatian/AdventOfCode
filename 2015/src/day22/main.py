import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def simulate(boss_hp, boss_dmg, p_hp, p_mana, effects, history, pturn):
    if pturn:
        if not history:
            return
        print('-- Player turn --')
        armor = 7 if 'Shield' in [name for name, _ in effects] else 0
        print(f'- Player has {p_hp} hit points, {armor} armor, {p_mana} mana')
        print(f'- Boss has {boss_hp} hit points')
        updated_effects = []
        for name, turns in effects:
            if turns > 0:
                updated_effects.append((name, turns - 1))

            if name == 'Poison':
                boss_hp -= 3
                print(f'Poison deals 3 damage; its timer is now {turns}')
            elif name == 'Recharge':
                p_mana += 101
                print(f'Recharge provides 101 mana; its timer is now {turns}')
            elif name == 'Shield':
                print(f'Shield\'s timer is now {turns}')

        name, (cost, turns) = history.pop(0)
        if name == 'Magic Missile':
            boss_hp -= 4
            print('Player casts Magic Missile, dealing 4 damage.')
        elif name == 'Drain':
            boss_hp -= 2
            p_hp += 2
            print('Player casts Drain, dealing 2 damage, and healing 2 hit points.')
        else:
            updated_effects.append((name, turns - 1))
            print(f'Player casts {name}')

        p_mana -= cost
        print()
        simulate(boss_hp, boss_dmg, p_hp, p_mana, updated_effects, history, not pturn)
    else:
        print('-- Boss turn --')
        armor = 7 if 'Shield' in [name for name, _ in effects] else 0
        print(f'- Player has {p_hp} hit points, {armor} armor, {p_mana} mana')
        print(f'- Boss has {boss_hp} hit points')
        updated_effects = []
        for name, turns in effects:
            if turns > 0:
                updated_effects.append((name, turns - 1))

            if name == 'Poison':
                boss_hp -= 3
                print(f'Poison deals 3 damage; its timer is now {turns}')
            elif name == 'Recharge':
                p_mana += 101
                print(f'Recharge provides 101 mana; its timer is now {turns}')
            elif name == 'Shield':
                print(f'Shield\'s timer is now {turns}')
        if 'Shield' in [name for name, _ in updated_effects]:
            p_hp -= 2
            print(f'Boss attacks for 9 - 7 = 2 damage')
        else:
            p_hp -= 9
            print(f'Boss attacks for 9 damage')

        print()
        simulate(boss_hp, boss_dmg, p_hp, p_mana, updated_effects, history, not pturn)


def pturn(boss_hp, boss_dmg, p_hp, p_mana, spells, effects, spent, best, history, lose_hp=False):
    if lose_hp:
        p_hp -= 1

    if p_hp <= 0:
        return 1e9
    if boss_hp <= 0:
        return spent

    updated_effects = []
    active_effects = []
    for name, turns in effects:
        if name == 'Poison':
            boss_hp -= 3
        elif name == 'Recharge':
            p_mana += 101

        if turns > 0:
            updated_effects.append((name, turns - 1))
            active_effects.append(name)

    options = []
    for spell, (cost, turns) in spells.items():
        if spell in active_effects:
            continue
        if cost <= p_mana:
            if spent + cost >= best:
                continue
            if spell == 'Magic Missile':
                boss_hp -= 4
            elif spell == 'Drain':
                boss_hp -= 2
                p_hp += 2
            else:
                updated_effects.append((spell, turns - 1))

            p_mana -= cost
            history.append((spell, (cost, turns)))
            result = bturn(boss_hp, boss_dmg, p_hp, p_mana, spells,
                    updated_effects, spent + cost, best, history, lose_hp)
            if result < best:
                best = result
                #print(best, history)

            history.pop()
            p_mana += cost

            if spell == 'Magic Missile':
                boss_hp += 4
            elif spell == 'Drain':
                boss_hp += 2
                p_hp -= 2
            else:
                updated_effects.pop()
            
    return best


def bturn(boss_hp, boss_dmg, p_hp, p_mana, spells, effects, spent, best, history, lose_hp=False):
    if p_hp <= 0:
        return -1
    if boss_hp <= 0:
        return spent

    effect_names = [name for name, turns in effects]
    if 'Shield' in effect_names:
        p_hp -= max(1, boss_dmg - 7)
    else:
        p_hp -= boss_dmg

    updated_effects = []
    for name, turns in effects:
        if name == 'Poison':
            boss_hp -= 3
        elif name == 'Recharge':
            p_mana += 101

        if turns > 0:
            updated_effects.append((name, turns - 1))

    return pturn(boss_hp, boss_dmg, p_hp, p_mana, spells, updated_effects, spent, best, history, lose_hp)


def part1(boss_hp, boss_dmg, p_hp, p_mana, spells):
    effects = []
    return pturn(boss_hp, boss_dmg, p_hp, p_mana, spells, effects, 0, 1e9, [])


def part2(boss_hp, boss_dmg, p_hp, p_mana, spells):
    effects = []
    return pturn(boss_hp, boss_dmg, p_hp, p_mana, spells, effects, 0, 1e9, [], True)


def main():
    with open(f'{dir_path}/../../inputs/day22/input') as f:
        inp = list(map(lambda x: int(x.split()[-1]), f.read().strip().split('\n')))

    spells = {
        "Magic Missile": (53, 0),
        "Drain": (73, 0),
        "Shield": (113, 6),
        "Poison": (173, 6),
        "Recharge": (229, 5)
    }
    print(inp)
    
    print(part1(*inp[:], 50, 500, spells))
    print(part2(*inp[:], 50, 500, spells))
    #simulate(*inp[:], 50, 500, [], [('Poison', (173, 6)), ('Magic Missile', (53, 0)), ('Recharge', (229, 5)), ('Poison', (173, 6)), ('Shield', (113, 6)), ('Recharge', (229, 5)), ('Poison', (173, 6)), ('Drain', (73, 0)), ('Magic Missile', (53, 0))], True)


if __name__ == '__main__':
    main()

