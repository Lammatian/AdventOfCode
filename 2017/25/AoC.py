with open("input.txt") as f:
	m = f.read().split("\n\n")

tape = [0]
current = 0
total = 0

state = ord(m[0].split('\n')[0].split(' ')[-1][0]) - ord('A')
steps = int(m[0].split('\n')[1].split(' ')[-2])

calls = [(1, 4), (2, 0), (3, 2), (4, 5), (0, 2), (4, 0)]

def call(state):
	global tape, current
	if tape[current] == 0:
		tape[current] = 1
		if state == 0:
			current += 1
			if len(tape) <= current:
				tape.append(0)
		else:
			current -= 1
			if current < 0:
				tape.insert(0, 0)
				current += 1
		return calls[state][0]
	elif tape[current] == 1:
		if state not in [4,5]:
			tape[current] = 0
		if state in [0, 3, 4]:
			current -= 1
			if current < 0:
				tape.insert(0, 0)
				current += 1
		else:
			current += 1
			if len(tape) <= current:
				tape.append(0)
		return calls[state][1]

if __name__ == '__main__':
	while total < steps:
		state = call(state)
		total += 1

	print(sum(tape))
