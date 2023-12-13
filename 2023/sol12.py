from functools import cache

def fits(p, g):
    if len(p) < g:
        return False
    elif len(p) == g:
        return all([x in '#?' for x in p])
    elif p[g] == '#':
        return False
    else:
        return all([x in '#?' for x in p[:g]])

@cache
def arrangements(p, g):
    if not p and not g:
        return 1
    if not p:
        return 0
    if not g:
        return 1 if all([x in '.?' for x in p]) else 0
    if p[0] == '.':
        return arrangements(p[1:], g)
    if p[0] == '?':
        if fits(p, g[0]):
            return arrangements(p[g[0] + 1:], g[1:]) + arrangements(p[1:], g)
        else:
            return arrangements(p[1:], g)
    if p[0] == '#':
        if fits(p, g[0]):
            return arrangements(p[g[0] + 1:], g[1:])
        else:
            return 0

def parseLine(l):
    p, g = l.split()
    g = tuple(map(int, g.split(',')))
    return p, g

def expand(record):
    p, g = record
    newp = '?'.join([p for _ in range(5)])
    newg = 5 * g
    return newp, newg

with open('aoc2023/inputs/day12/input.txt') as f:
    records = list(map(parseLine, f.read().splitlines()))

expandedRecords = list(map(expand, records))
print(sum(map(lambda x: arrangements(*x), records)))
print(sum(map(lambda x: arrangements(*x), expandedRecords)))
