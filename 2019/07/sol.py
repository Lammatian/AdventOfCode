import sys
from itertools import permutations

class Intcode():
    def __init__(self, ins, inputs):
        self.ins = ins
        self.cur = 0

        self.inputs = inputs
        self.cur_inp = 0

        self.outputs = []

    def simulate(self):
        while self.cur >= 0:
            self.cur = self.parse_command()

        return self.outputs

    def done(self):
        return self.cur < 0

    def execute_command(self):
        """
        Parse and execute the command, updating current pointer
        If command produced an output, return it
        """
        out_len = len(self.outputs)
        self.cur = self.parse_command()
        
        if len(self.outputs) > out_len:
            return self.outputs[-1]

    def add_input(self, new_input):
        self.inputs.append(new_input)

    def parse_command(self):
        """
        Parse command and return next position
        If no next position, return -1
        """
        op_code = self.ins[self.cur]
        op = op_code % 10
        modes = [self.digit(op_code, i) for i in range(2, 4)]

        if op < 3: # add/mul
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, 1)
            result = a1 + a2 if op == 1 else a1 * a2
            self.ins[a3] = result
            return self.cur + 4
        elif op == 3: # input
            if self.cur_inp >= len(self.inputs):
                return self.cur

            a1 = self.get_val(self.cur + 1, 1)
            self.ins[a1] = self.inputs[self.cur_inp]
            self.cur_inp += 1
            return self.cur + 2
        elif op == 4: # output
            a1 = self.get_val(self.cur + 1, modes[0])
            #print('Output:', a1)
            self.outputs.append(a1)
            return self.cur + 2
        elif op == 5: # jump if true
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])

            if a1 != 0:
                return a2
            
            return self.cur + 3
        elif op == 6: # jump if false
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])

            if a1 == 0:
                return a2
            
            return self.cur + 3
        elif op == 7: # set less than
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, 1)

            if a1 < a2:
                self.ins[a3] = 1
            else:
                self.ins[a3] = 0

            return self.cur + 4
        elif op == 8: # set equals
            a1 = self.get_val(self.cur + 1, modes[0])
            a2 = self.get_val(self.cur + 2, modes[1])
            a3 = self.get_val(self.cur + 3, 1)

            if a1 == a2:
                self.ins[a3] = 1
            else:
                self.ins[a3] = 0

            return self.cur + 4
        else: # 99 = end
            return -1
    
    def get_val(self, pos, mode):
        """
        Get argument value based on the mode
        If mode = 0, gets ins[ins[pos]]
        If mode = 1, gets ins[pos]
        """
        if mode == 0:
            return self.ins[self.ins[pos]]
        if mode == 1:
            return self.ins[pos]

    @staticmethod
    def digit(n, i):
        """
        Get i-th digit of n
        """
        if i >= len(str(n)):
            return 0

        return int(str(n)[-(i + 1)])


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

    return best, best_perm


def sol2(ins):
    best = 0
    best_perm = []
    perm_count = 0

    for perm in permutations(range(5, 10)):
    # for perm in [(5, 6, 7, 8, 9)]:
        initial_inputs = [[perm[i]] for i in range(5)]
        initial_inputs[0].append(0)

        programs = [Intcode(ins[:], int_inp) for int_inp in initial_inputs]

        finished = [False] * 5
        last_out = None
        last_meaningful_out = None

        c = 0
        while not all([p.done() for p in programs]):# and c < 30:
            # Simulate one step of each
            for i, program in enumerate(programs):
                if program.done():
                    finished[i] = True

                    if all(finished):
                        return last_out

                    continue

                if last_out is not None:
                    last_meaningful_out = last_out
                    program.add_input(last_out)

                last_out = program.execute_command()

            # print([p.cur for p in programs])
            c += 1

        # print(last_meaningful_out)
        
        if last_meaningful_out and last_meaningful_out > best:
            best = last_meaningful_out
            best_perm = perm

        perm_count += 1
        print(perm_count)

    return best, best_perm

def main():
    with open(sys.argv[1]) as f:
        instructions = list(map(int, f.read().split(',')))

    # print('First:', sol1(instructions[:]))
    print('Second:', sol2(instructions[:]))


if __name__ == '__main__':
    main()
