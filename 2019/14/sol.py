import sys
from math import ceil
from collections import defaultdict
from collections import OrderedDict

class Reaction:
    def __init__(self, string):
        out, inp = self.parse_reaction(string)

        self.out_quantity = out[0]
        self.out_chemical = out[1]

        self.inp_chemicals = {}

        for inp_q, inp_c in inp:
            self.inp_chemicals[inp_c] = inp_q

    def parse_reaction(self, reaction):
        inp, out = reaction.split('=>')
        inp = inp.strip().split(',')
        inp = tuple(map(lambda x: x.strip().split(' '), inp))
        inp = tuple(map(lambda x: (int(x[0]), x[1]), inp))

        out = out.strip().split(' ')
        out = (int(out[0]), out[1])

        return out, inp

    def reverse(self, quantity):
        """
        Given the requirement of getting `quantity` of the
        output chemical, return the list of all the input
        chemicals and their corresponding quantities to
        create the output

        If the reaction produces more than required, return
        also the leftover quantity
        """
        batches = ceil(quantity / self.out_quantity)
        leftover = self.out_quantity * batches - quantity

        result = []

        for inp_c, inp_q in self.inp_chemicals.items():
            result.append((inp_c, batches * inp_q))

        return result, leftover


class Reactions:
    def __init__(self, data):
        self.r_dict = {}

        for r_str in data:
            r = Reaction(r_str)
            self.r_dict[r.out_chemical] = r 

    def calculate_ore(self, fuel=1):
        ore = 0
        required = OrderedDict()
        required['FUEL'] = fuel
        leftovers = defaultdict(int)

        while required:
            chemical, quantity = required.popitem(last=False)

            reaction = self.r_dict[chemical]
            inputs, leftover = reaction.reverse(quantity - leftovers[chemical])
            leftovers[chemical] = leftover

            for inp_c, inp_q in inputs:
                if inp_c == 'ORE':
                    ore += inp_q
                    continue

                required[inp_c] = required.get(inp_c, 0) + inp_q

        return ore

    def use_all_ore(self, ore=1000000000000):
        fuel = 0
        jump_size = 1000000

        while not (self.calculate_ore(fuel) <= ore and self.calculate_ore(fuel + 1) > ore):
            while self.calculate_ore(fuel) <= ore:
                fuel += jump_size

            fuel -= jump_size
            jump_size //= 2

        return fuel


def sol1(data):
    reactions = Reactions(data)
    return reactions.calculate_ore()


def sol2(data):
    reactions = Reactions(data)
    return reactions.use_all_ore()


def main():
    with open(sys.argv[1]) as f:
        data = f.readlines()

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()