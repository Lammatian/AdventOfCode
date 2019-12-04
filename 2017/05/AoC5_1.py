with open("input5.txt") as f:
	m = list(map(int, f.read().split('\n')))

current = 0
steps = 0
print(len(m))

while current >= 0 and current < len(m):
	temp = current
	current += m[current]
	m[temp] += 1
	steps += 1

print(steps)