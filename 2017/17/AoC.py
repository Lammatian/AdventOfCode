step = 354

arr = [0]
currpos = 0
after = 0

for i in range(1, 50000001):
	currpos = (currpos + step)%i + 1
	if currpos == 1:
		after = i

print(after)