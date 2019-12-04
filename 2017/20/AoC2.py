import re

with open("input.txt") as f:
	m = list(map(lambda x: re.findall("<([^=]*)>", x), f.readlines()))

for i, line in enumerate(m):
	m[i] = list(map(lambda x: list(map(int, x.split(','))), line))

positions = {}

def getpos(m):
	return set([tuple(x) for (x,v,a) in m])

def getcollisions(m):
	to_pop = set()

	for i in range(len(m)):
		if i in to_pop:
			continue

		for j in range(i+1, len(m)):
			if m[i][0] == m[j][0]:
				to_pop.add(i)
				to_pop.add(j)

	return sorted(list(to_pop))


def cancollide(x1, v1, a1, x2, v2, a2):
	for i in range(3):
		if x1[i] < x2[i] and v1[i] <= v2[i] and a1[i] <= a2[i]:
			return False
		elif x1[i] > x2[i] and v1[i] >= v2[i] and a1[i] >= a2[i]:
			return False
	else:
		return True

def poscol(m):
	total = 0

	for i in range(len(m)):
		for j in range(i+1, len(m)):
			if cancollide(*m[i], *m[j]):
				total += 1

	return total

for t in range(1000):
	if t%100 == 0:
		if poscol(m) == 0:
			print("No more", len(m))
			break
	# update all particles
	if len(getpos(m)) < len(m):
		to_pop = getcollisions(m)
		for n in to_pop[::-1]:
			m.pop(n)

	for i, (x,v,a) in enumerate(m):
		m[i][1] = [a+b for a,b in zip(v,a)]
		m[i][0] = [a+b for a,b in zip(x,m[i][1])]

# collisions = {}
# total = 0

# for i in range(1000):
# 	for j in range(i+1, 1000):
# 		if cancollide(*m[i], *m[j]):
# 			if collisions.get(i):
# 				collisions[i].append(j)
# 			else:
# 				collisions[i] = [j]
# 			total += 1

# print(collisions)
# print("Possible collisions:", total)

# print(m[195])
# print(m[900])