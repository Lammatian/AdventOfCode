with open("input.txt") as f:
	m = list(map(lambda x: x.split("<->"), f.read().split('\n')))

m = dict([[int(x[0]), list(map(int, x[1].split(",")))] for x in m])

visited = [False]*len(m)

def visitall(start):
	global visited, m
	stack = [start]
	while stack:
		popped = stack.pop(0)
		visited[popped] = True

		stack.extend([x for x in m[popped] if not visited[x]])

groups = 0

for n in range(2000):
	if not visited[n]:
		visitall(n)
		groups += 1

print(groups)