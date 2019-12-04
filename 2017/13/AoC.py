from functools import reduce
from math import gcd

with open("input.txt") as f:
	m = list(map(lambda x: list(map(int, x.split(':'))), f.read().split('\n')))

print(m)

def getpos(delay, m):
	return [(x[0]+delay)%(2*(x[1]-1)) for x in m]

def help(m):
	return [2*(x[1]-1) for x in m]

severity = 0

for layer in m:
	if layer[0]%(2*(layer[1]-1)) == 0:
		severity += layer[0]*layer[1]

print(severity)

# slow
# i = 0
# while True:
# 	if all(getpos(i, m)):
# 		print(i)
# 		break
# 	i += 1

print(help(m))
print(reduce(lambda x,y: x*y, help(m))//2)