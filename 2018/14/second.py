inp = "768071"
#inp = "59414"

current = "37"
c1 = 0
c2 = 1

while current[-len(inp):] != inp and current[-len(inp)-1:-1] != inp:
    n = int(current[c1]) + int(current[c2])
    current += str(n)

    c1 = (c1 + int(current[c1]) + 1) % len(current)
    c2 = (c2 + int(current[c2]) + 1) % len(current)

print(current.index(inp))