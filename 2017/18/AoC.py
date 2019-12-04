from time import sleep

from collections import defaultdict

with open("input.txt") as f:
	inst = list(map(lambda x: x.split(), f.read().split('\n')))

last_sound = 0
curr = 0
received = -1
regs = defaultdict(int)

def set(r, v):
	regs[r] = regs[v] if v.isalpha() else int(v)

def mul(r, v):
	regs[r] *= regs[v] if v.isalpha() else int(v)

def add(r, v):
	regs[r] += regs[v] if v.isalpha() else int(v)

def mod(r, v):
	regs[r] %= regs[v] if v.isalpha() else int(v)

def snd(r):
	global last_sound
	last_sound = regs[r]

def rcv(r):
	return last_sound if regs[r] > 0 else None

def jgz(r, v):
	global curr
	if r.isalpha() and regs[r] > 0:
		curr += regs[v]-1 if v.isalpha() else int(v)-1
	elif not r.isalpha() and int(r) > 0:
		curr += regs[v]-1 if v.isalpha() else int(v)-1

while curr < len(inst) and received < 0:
	i, r, v, *rest = inst[curr] + [""]
	if i == "set":
		set(r, v)
	if i == "mul":
		mul(r, v)
	if i == "add":
		add(r, v)
	if i == "mod":
		mod(r, v)
	if i == "snd":
		snd(r)
	if i == "rcv":
		ret = rcv(r)
		if ret is not None:
			print(ret)
			break
	if i == "jgz":
		jgz(r, v)
	curr += 1