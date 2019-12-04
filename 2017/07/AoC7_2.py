with open("input7.txt") as f:
	m = f.read().split('\n')

m = list(map(lambda x: x.split('->'), m))

for i, line in enumerate(m):
	m[i][0] = line[0].split()
	m[i][0][1] = int(m[i][0][1][1:-1])
	if len(line) > 1:
		m[i][1] = list(map(lambda x: x.strip(), line[1].split(',')))

weights = {}

while m:
	line = m.pop(0)

	if len(line) == 1:
		weights[line[0][0]] = line[0][1]
	elif set(line[1]).issubset(set(weights.keys())):
		if all([weights[x] == weights[line[1][0]] for x in line[1]]):
			weights[line[0][0]] = line[0][1] + sum([weights[x] for x in line[1]])
		else:
			print([(x, weights[x]) for x in line[1]])
	else:
		m.append(line)

print(weights)