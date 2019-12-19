import sys
sys.path.append('..')
from itertools import permutations
from util.intcode import Intcode

def sol1(ins):
    best = 0
    best_perm = []

    for perm in permutations(range(5)):
        amp_a = Intcode(ins[:], [perm[0], 0])
        out_a = amp_a.simulate()[0]

        amp_b = Intcode(ins[:], [perm[1], out_a])
        out_b = amp_b.simulate()[0]

        amp_c = Intcode(ins[:], [perm[2], out_b])
        out_c = amp_c.simulate()[0]

        amp_d = Intcode(ins[:], [perm[3], out_c])
        out_d = amp_d.simulate()[0]

        amp_e = Intcode(ins[:], [perm[4], out_d])
        out_e = amp_e.simulate()[0]

        if out_e > best:
            best = out_e
            best_perm = perm

    return best


def sol2(ins):
    best = 0
    best_perm = []
    perm_count = 0

    for perm in permutations(range(5, 10)):
        initial_inputs = [[perm[i]] for i in range(5)]
        initial_inputs[0].append(0)

        programs = [Intcode(ins[:], int_inp) for int_inp in initial_inputs]

        finished = [False] * 5
        last_out = None
        last_meaningful_out = None

        while not all([p.done() for p in programs]):
            # Simulate one step of each
            for i, program in enumerate(programs):
                if program.done():
                    finished[i] = True

                    if all(finished):
                        return last_out

                    continue

                if last_out is not None and last_out >= 0:
                    last_meaningful_out = last_out
                    program.add_input(last_out)

                last_out = program.execute_command()

        if last_meaningful_out and last_meaningful_out > best:
            best = last_meaningful_out
            best_perm = perm

    return best


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))

    print(sol1(data[:]))
    print(sol2(data[:]))


if __name__ == '__main__':
    main()
