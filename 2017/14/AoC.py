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

used_squares = 0

for i in range(128):
	used_squares += sum([x == "1" for x in knot(inp + "-" + str(i))])

print(knot(inp))
print(used_squares)