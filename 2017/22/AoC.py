with open("input.txt") as f:
	m = f.readlines()

mapsize = 401

m = [list(x.strip()) for x in m]

half = (mapsize - len(m))//2

n = [list("."*mapsize) for i in range(half)]

for i in range(len(m)):
	n.append(list(".")*half + m[i] + list(".")*half)

for i in range(half):
	n.append(list("."*mapsize))

m = n

row = mapsize//2
col = mapsize//2

infections = 0

dirs = ["L", "U", "R", "D"]
d = 1

for i in range(10000):
	if m[row][col] == "#":
		d = (d+1)%4
		m[row][col] = "."
	else:
		d = (d-1)%4
		m[row][col] = "#"
		infections += 1

	if dirs[d] == "L":
		col -= 1
	elif dirs[d] == "R":
		col += 1
	elif dirs[d] == "U":
		row -= 1
	else:
		row += 1

print(infections)