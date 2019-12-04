with open("input.txt") as f:
	number = f.read()

result = 0

for i in range(-len(number), 0):
	if number[i] == number[i+1]:
		result += int(number[i])

print(result)