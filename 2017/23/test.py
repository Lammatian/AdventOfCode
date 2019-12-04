b = 108100
c = 125100
h = 0

while b != c:
	f, d = 1, 2

	while d-b != 0:
		if b%d == 0:
			f = 0
			break

		d += 1

	if f == 0:
		h += 1

	b += 17

print(h)

# b = 108100
# c = 125100
# h = 0
# while b != c:
# 	for d in range(2, b):
# 		for e in range(2, b):
# 			if d*e == b:
# 				f = 0

# 	if f == 0: # if found a pair, increase (ie. if not prime, increase)
# 		h += 1

# 	b += 17
# print(h)

def prime(n):
	if n < 2:
		return False
	if n == 2:
		return True
	if n%2 == 0:
		return False
	else:
		for i in range(3, int(n**(1/2))+1, 2):
			if n%i == 0:
				return False
	return True

h = 0
for i in range(108100, 125100, 17):
	if not prime(i):
		h += 1

print(h)