with open("input.txt") as f:
	m = list(map(lambda x: list(map(int, x.split('\t'))), f.readlines()))

result = 0
new = False

for row in m:
	for i in range(0, len(row)):
		for j in range(i+1, len(row)):
			if row[i]%row[j] == 0:
				result += row[i]//row[j]
				new = True
			elif row[j]%row[i] == 0:
				result += row[j]//row[i]
				new = True
			if new:
				break
		if new:
			new = False
			break

print(result)