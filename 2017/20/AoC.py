import re

with open("input.txt") as f:
	m = list(map(lambda x: re.findall('<([^=]*)>', x), f.readlines()))

for i in range(len(m)):
	m[i] = list(map(lambda x: list(map(int, x.split(','))), m[i]))

min_a = sum(map(abs, m[0][2]))
min_list = [0]

def can_collide(x1, v1, a1, x2, v2, a2):
	for i in range(3):
		if x1[i] < x2[i] and v1[i] < v2[i] and a1[i] < a2[i]:
			return False
		elif x1[i] > x2[i] and v1[i] > v2[i] and a1[i] > a2[i]:
			return False
	else:
		return True

for t in range(100):
	# check collisions
	to_pop = []

	for i in range(len(m)):
		for j in range(i+1, len(m)):
			if j in to_pop:
				continue
			if m[i][0] == m[j][0]:
				to_pop.extend([i,j])

	for p in to_pop:
		m.pop(p)

	# update positions
	for i, (x,v,a) in enumerate(m):
		m[i][0] = [a+b for a,b in zip(x,v)]
		m[i][1] = [a+b for a,b in zip(v,a)]

print(len(m))