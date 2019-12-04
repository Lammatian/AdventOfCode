with open("input.txt") as f:
	m = f.read().split('\n')

row = 0
col = m[0].index('|')
maxRow = len(m) - 1
maxCol = len(m[0]) - 1

d = 1 # 1 - vertical, -1 - horizontal
down = True
right = True
letters = []
steps = 1

while m[row][col] != ' ':
	print(m[row][col], d, (row, col))
	if m[row][col] == '+':
		d *= -1
		if d == 1:
			down = m[min(row+1, maxRow)][col] == '|'
		else:
			right = m[row][min(col+1, maxCol)] == '-'
	if d == 1:
		if down:
			row += 1
		else:
			row -= 1
	elif d == -1:
		if right:
			col += 1
		else:
			col -= 1
	if m[row][col] not in ['-', '|', '+']:
		letters.append(m[row][col])
	steps += 1

print(''.join(letters).strip())
print(steps)