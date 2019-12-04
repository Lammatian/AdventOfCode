from functools import reduce

inp = "oundnydw"

def knot(s):
	ls = list(s.encode()) + [17, 31, 73, 47, 23]
	code = list(range(256))
	current = 0
	skip = 0

	for _ in range(64):
		for l in ls:
			selection = [code[(current + i)%len(code)] for i in range(l)]
			rev = list(reversed(selection))

			for i in range(l):
				code[(current + i)%len(code)] = rev[i]

			current += (l + skip)%len(code)
			skip += 1

	groups = [code[i:i+16] for i in range(0, 256, 16)]
	dense = [reduce(lambda x,y: x^y, group) for group in groups]

	hexknot = ''.join(map(lambda x: hex(x)[2:].zfill(2), dense))

	return bin(int(hexknot, 16))[2:].zfill(128)

grid = [knot(inp + "-" + str(i)) for i in range(128)]
print(grid)

def adjacent(x, y):
	return set([(max(x-1, 0), y), (min(x+1, 127), y), (x, max(y-1, 0)), (x, min(y+1, 127))]) - {(x,y)}

print(adjacent(127, 127))

def groups(grid):
	groups = 0
	stack = []

	visited = []
	for i in range(128):
		visited.append([False]*128)

	for row in range(128):
		for column in range(128):
			if grid[row][column] == "1" and not visited[row][column]:
				groups += 1
				stack.append((row, column))

				while stack:
					popped = stack.pop(0)
					visited[popped[0]][popped[1]] = True

					for adj in adjacent(*popped):
						if grid[adj[0]][adj[1]] == "1" and not visited[adj[0]][adj[1]]:
							stack.append(adj)

	return groups

print(groups(grid))
