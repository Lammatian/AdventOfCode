from collections import Counter

with open("input4.txt") as f:
	m = f.read().split('\n')[:-1]

passes = [x.split(' ') for x in m]
passes = [list(map(''.join, map(sorted, x))) for x in passes]
result = 0

for line in passes:
	print(line)
	if len(set(line)) == len(line):
		result += 1

print(result)
