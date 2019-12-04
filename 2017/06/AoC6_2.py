from copy import copy

with open("input6.txt") as f:
	m = list(map(int, f.read().split('\t')))

l = len(m)
seen = []

while not m in seen:
	seen.append(copy(m))
	next = [max(m)//l + 1] * (max(m)%l) + [max(m)//l] * (l - max(m)%l)
	imax = m.index(max(m))
	m[m.index(max(m))] = 0

	for i in range(l):
		m[imax - l + 1 + i] += next[i]

	print(m)

print(len(seen) - seen.index(m)) 