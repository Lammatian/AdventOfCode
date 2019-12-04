from copy import deepcopy

with open("input.txt") as f:
	m = list(map(lambda x: list(map(int, x.split('/'))), f.readlines()))

d = {}

for a,b in m:
	if d.get(a):
		d[a].append(b)
	else:
		d[a] = [b]

	if d.get(b):
		d[b].append(a)
	else:
		d[b] = [a]

def findmax(dominos, end, total, bridge):
	if not dominos.get(end):
		# print(bridge)
		# return total - part 1
		return (len(bridge), sum([a+b for a,b in bridge]))
	else:
		dicts = []
		ends = []

		for v in dominos[end]:
			nd = deepcopy(dominos)
			nd[end].pop(nd[end].index(v))
			nd[v].pop(nd[v].index(end))
			dicts.append(nd)
			ends.append(v)

		return max((0,0), *[findmax(dicts[i], ends[i], total + end + ends[i], bridge + [(end, ends[i])]) for i in range(len(dicts))])


# _max = 0
maxlen = 0
maxstr = 0
for val in d[0]:
	nd = deepcopy(d)
	nd[0].pop(nd[0].index(val))
	nd[val].pop(nd[val].index(0))
	# _max = max(_max, findmax(nd, val, val, [(0, val)]))
	l, s = findmax(nd, val, val, [(0, val)])
	if l > maxlen:
		maxlen = l
		maxstr = s
	elif l == maxlen and s > maxstr:
		maxlen = l
		maxstr = s

print(maxstr)