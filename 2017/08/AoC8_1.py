from collections import defaultdict

with open("input8.txt") as f:
	m = list(map(lambda x: x.split(), f.read().split('\n')))[:-1]

regs = defaultdict(int)

for line in m:
	print(line[4:7])
	if eval("regs['" + line[4] + "']" + ' '.join(line[5:7])):
		if line[1] == "inc":
			regs[line[0]] += int(line[2])
		else:
			regs[line[0]] -= int(line[2])

print(max(regs.values()))