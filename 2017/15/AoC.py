sa = 618
sb = 814
fa = 16807
fb = 48271
print(sa)
a = sa
b = sb
same = 0

for i in range(10):
	a = (a*fa)%(2**31 - 1)
	b = (b*fb)%(2**31 - 1)
	print(a,b)
	if a%65536 == b%65536:
		same += 1

print(same)