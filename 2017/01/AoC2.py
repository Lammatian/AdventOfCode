with open("input.txt") as f:
	m = f.read()

jump = len(m)//2

result = 0

for i in range(-len(m), -len(m)//2):
	if m[i] == m[i+jump]:
		result += 2*int(m[i])

print(result)