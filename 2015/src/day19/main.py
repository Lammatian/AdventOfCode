import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from queue import Queue
from collections import defaultdict
from functools import lru_cache


def part1(rules, molecule):
    distinct = set()

    for f, t in rules:
        for i in range(len(molecule) - len(f) + 1):
            if molecule[i:i+len(f)] == f:
                distinct.add(molecule[:i] + t + molecule[i+len(f):])

    return len(distinct)


def lol(molecule, rules):
    rev_rules = defaultdict(list)
    for f, t in rules:
        rev_rules[t].append(f)
    steps = 0
    queue = Queue()
    queue.put((molecule, steps))
    seen = set()
    seen.add(molecule)

    while True:
        molecule, steps = queue.get()

        for t, v in rev_rules.items():
            out = False
            for i in range(len(molecule) - len(f) + 1):
                if molecule[i:i+len(t)] == t:
                    for f in v:
                        reverted = revert(molecule, (f, t), i)
                        if len(reverted) == 1:
                            return steps + 1, reverted
                        if reverted not in seen:
                            seen.add(reverted)
                            queue.put((reverted, steps + 1))
                    out = True
                    break
            if out:
                break


def revert(molecule, rule, pos):
    return molecule[:pos] + (rule[0],) + molecule[pos + len(rule[1]):]


def part2_(rules, medicine):
    starts = []
    ignores = []
    if medicine == 'e':
        return 0

    for i, atom in enumerate(medicine):
        if atom == 'Rn':
            if ignores:
                ignores.pop()
                continue

            starts.append(i)
        elif atom == 'Ar':
            if i + 1 < len(medicine) and  medicine[i + 1] == 'Rn':
                ignores.append(True)
                continue

            s = starts.pop()
            print('From', s - 1, 'to', i + 1)
            print(medicine[s - 1:i + 1])
            if s - 1 >= 0 and medicine[s - 1] == 'Ca':
                steps, reverted = lol(medicine[s - 2:i + 1], rules)
                print(steps, reverted)
                medicine = medicine[:s-2] + reverted + medicine[i+1:]
                print('Resulting medicine', medicine)
                return steps + part2_(rules, medicine)
            else:
                steps, reverted = lol(medicine[s - 1:i + 1], rules)
                print(steps, reverted)
                medicine = medicine[:s-1] + reverted + medicine[i+1:]
                print('Resulting medicine', medicine)
                return steps + part2_(rules, medicine)
    else:
        return lol(medicine, rules)[0]


def parse_atoms(molecule):
    atoms = []
    last = 0
    i = 0
    while i < len(molecule) - 1:
        i += 1
        if molecule[i].isupper():
            atoms.append(molecule[last:i])
            last = i

    atoms.append(molecule[last:])
    return tuple(atoms)


def main():
    with open(f'{dir_path}/../../inputs/day19/input') as f:
        rules_str, molecule = f.read().strip().split('\n\n')

    molecule = parse_atoms(molecule)
    rules = [] 
    for i, rule in enumerate(rules_str.split('\n')):
        f, t = rule.split(' => ')
        rules.append([f, parse_atoms(t)])

    for rule in rules:
        print(rule)
    print(molecule)

    print(part1(rules[:], molecule[:]))
    print(part2_(rules[:], molecule[:]))


if __name__ == '__main__':
    main()

