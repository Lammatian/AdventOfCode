with open("input9.txt") as f:
	m = f.read()

i = 0
depth = 1
score = 0
ingarbage = False
garbagecount = 0

while i < len(m):
	if m[i] == '!':
		i += 1
	elif m[i] == '<' and not ingarbage:
		ingarbage = True
	elif m[i] == '>' and ingarbage:
		ingarbage = False
	elif ingarbage:
		garbagecount += 1
	elif m[i] == '{' and not ingarbage:
		score += depth
		depth += 1
	elif m[i] == '}' and not ingarbage:
		depth -= 1

	i += 1

print(score)
print(garbagecount)