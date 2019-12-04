from time import sleep

from collections import defaultdict

with open("input.txt") as f:
	inst = list(map(lambda x: x.split(), f.read().split('\n')))

curr = 0
regs = defaultdict(int)

def set(r, v):
	regs[r] = regs[v] if v.isalpha() else int(v)

def mul(r, v):
	regs[r] *= regs[v] if v.isalpha() else int(v)

def sub(r, v):
	regs[r] -= regs[v] if v.isalpha() else int(v)

def jnz(r, v):
	global curr
	if r.isalpha() and regs[r] != 0:
		curr += regs[v]-1 if v.isalpha() else int(v)-1
	elif not r.isalpha() and int(r) > 0:
		curr += regs[v]-1 if v.isalpha() else int(v)-1

mulcount = 0
while curr < len(inst):
	i, r, v, *rest = inst[curr] + [""]
	if i == "set":
		set(r, v)
	if i == "mul":
		mul(r, v)
		mulcount += 1
	if i == "sub":
		sub(r, v)
	if i == "jnz":
		jnz(r, v)
	curr += 1

print(mulcount)