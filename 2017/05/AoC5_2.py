with open("input5.txt") as f:
	m = list(map(int, f.read().split('\n')))

current = 0
steps = 0

while current >= 0 and current < len(m):
	temp = current
	current += m[current]
	m[temp] += 1 if m[temp] < 3 else -1
	steps += 1

print(steps)