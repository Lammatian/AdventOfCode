from collections import defaultdict

with open("input.txt") as f:
	inst = list(map(lambda x: x.split(), f.read().split('\n')))

curr = [0, 0]
sent = [[], []]
regs0 = defaultdict(int)
regs1 = defaultdict(int)
regs1['p'] = 1
regs = [regs0, regs1]

def set(p, r, v):
	regs[p][r] = regs[p][v] if v.isalpha() else int(v)

def mul(p, r, v):
	regs[p][r] *= regs[p][v] if v.isalpha() else int(v)

def add(p, r, v):
	regs[p][r] += regs[p][v] if v.isalpha() else int(v)

def mod(p, r, v):
	regs[p][r] %= regs[p][v] if v.isalpha() else int(v)

def snd(p, r):
	sent[p].append(regs[p][r])

def rcv(p, r):
	pass
	# handle separately

def jgz(p, r, v):
	if r.isalpha() and regs[p][r] > 0:
		curr[p] += regs[p][v]-1 if v.isalpha() else int(v)-1
	elif not r.isalpha() and int(r) > 0:
		curr[p] += regs[p][v]-1 if v.isalpha() else int(v)-1

p1send = 0
while inst[curr[0]][0] != "rcv" or inst[curr[1]][0] != "rcv" or len(sent[0]) > 0 or len(sent[1]) > 0:
	# program 0
	for p in [0, 1]:
		i, r, v, *rest = inst[curr[p]] + [""]
		if i == "set":
			set(p, r, v)
		if i == "mul":
			mul(p, r, v)
		if i == "add":
			add(p, r, v)
		if i == "mod":
			mod(p, r, v)
		if i == "snd":
			snd(p, r)
			if p == 1:
				p1send += 1
		if i == "jgz":
			jgz(p, r, v)
		if i == "rcv":
			if sent[-p+1]:
				regs[p][r] = sent[-p+1].pop(0)
			else:
				# wait
				curr[p] -= 1
		curr[p] += 1

print(p1send)