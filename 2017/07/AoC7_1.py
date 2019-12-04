with open("input7.txt") as f:
	m = f.read().split('\n')[:-1]

m = list(map(lambda x: x.split('->'), m))

for i, line in enumerate(m):
	if len(line) > 1:
		m[i][1] = list(map(lambda x: x.strip(), line[1].split(',')))

nottop = set()
discs = set()

for line in m:
	discs.add(line[0].split()[0])
	if len(line) > 1:
		nottop.update(line[1])

print(discs - nottop)