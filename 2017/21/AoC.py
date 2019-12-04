import numpy as np

def split3(arr):
	vert = np.vsplit(arr, len(arr)//3)
	subarrs = list(map(lambda x: np.hsplit(x, len(arr)//3), vert))
	return subarrs

def split2(arr):
	vert = np.vsplit(arr, len(arr)//2)
	subarrs = list(map(lambda x: np.hsplit(x, len(arr)//2), vert))
	return subarrs

def unsplit(subarrs):
	rows = [np.concatenate(row, axis=1) for row in subarrs]
	return np.concatenate(rows)

start = np.array([list(".#."), list("..#"), list("###")])

with open("input.txt") as f:
	m = f.readlines()

moves = {}

for move in m:
	move = move.split(" => ")
	base = np.array(list(map(lambda x: list(x), move[0].split("/"))))
	target = np.array(list(map(lambda x: list(x), move[1].strip().split("/"))))

	# rotations
	for i in range(0, 4):
		moves[str(np.rot90(base, k=i))] = target

	# flips
	for i in range(0, 4):
		moves[str(np.rot90(np.fliplr(base), k=i))] = target

for i in range(18):
	if len(start)%2 == 0:
		start = split2(start)
	else:
		start = split3(start)
	newStart = [list(map(lambda x: moves[str(x)], row)) for row in start]
	start = unsplit(newStart)
	print(np.unique(start, return_counts=True))