from collections import defaultdict

with open("input11.txt") as f:
	moves = f.read().split(',')

currentpos = [0, 0, 0]
maxdist = 0

for m in moves:
	if m == "n":
		currentpos[1] += 1
		currentpos[2] -= 1
	if m == "ne":
		currentpos[0] += 1
		currentpos[2] -= 1
	if m == "se":
		currentpos[0] += 1
		currentpos[1] -= 1
	if m == "s":
		currentpos[1] -= 1
		currentpos[2] += 1
	if m == "sw":
		currentpos[0] -= 1
		currentpos[2] += 1
	if m == "nw":
		currentpos[0] -= 1
		currentpos[1] += 1

	maxdist = max(maxdist, sum(map(abs, currentpos))//2)

print(currentpos)
print(sum(map(abs, currentpos))//2)
print(maxdist)