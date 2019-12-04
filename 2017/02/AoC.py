with open("input.txt") as f:
	m = list(map(lambda x: list(map(int, x.split("\t"))), f.readlines()))

total = [max(row)-min(row) for row in m]

print(sum(total))