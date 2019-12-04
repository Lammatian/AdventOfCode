with open("input10.txt") as f:
	ls = map(int, f.read().split(','))

code = list(range(256))
current = 0
skip = 0

for l in ls:
	selection = [code[(current + i)%len(code)] for i in range(l)]
	rev = list(reversed(selection))
	for i in range(l):
		code[(current + i)%len(code)] = rev[i]

	current += l + skip
	skip += 1

print(code)
print(code[0]*code[1])