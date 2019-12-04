with open("input4.txt") as f:
	m = f.read()

passes = m.split('\n')[:-1]
passes = [x.split() for x in passes]
unique = sum([len(set(x)) == len(x) for x in passes])
print(unique)