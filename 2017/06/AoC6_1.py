with open("input6.txt") as f:
	m = list(map(int, f.read().split('\t')))

l = len(m)
seen = set()
next = tuple()
loops = 0

while not tuple(m) in seen:
	seen.add(tuple(m))
	next = [max(m)//l + 1] * (max(m)%l) + [max(m)//l] * (l - max(m)%l)
	imax = m.index(max(m))
	m[m.index(max(m))] = 0

	for i in range(l):
		m[imax - l + 1 + i] += next[i]

	print(m)

	loops += 1

print(loops) 