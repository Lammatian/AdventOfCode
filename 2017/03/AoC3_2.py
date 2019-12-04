matrix = []

for i in range(11):
	matrix.append([0]*11)

matrix[5][5] = 1

r = 5
c = 5

def sumAdjacent(matrix, r, c):
	return sum(matrix[r-1][c-1:c+2]) + sum(matrix[r][c-1:c+2]) + sum(matrix[r+1][c-1:c+2]) - matrix[r][c]

for i in range(1, 10):
	if i%2 == 1:
		# right -> top
		for j in range(i):
			#right 
			c += 1
			matrix[r][c] = sumAdjacent(matrix, r, c)
		for j in range(i):
			# top
			r -= 1
			matrix[r][c] = sumAdjacent(matrix, r, c)
	else:
		# left -> bottom
		for j in range(i):
			# left
			c -= 1
			matrix[r][c] = sumAdjacent(matrix, r, c)
		for j in range(i):
			# bottom
			r += 1
			matrix[r][c] = sumAdjacent(matrix, r, c)

for row in matrix:
	print(row)