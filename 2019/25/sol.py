import sys
sys.path.append('..')
from util.intcode import Intcode
from itertools import combinations


# Scientifically derived
# We ignore the items that make us lose
COLLECT_ALL_ITEMS = """
south
take spool of cat6
west
take space heater
south
take shell
north
north
take weather machine
west
south
east
take candy cane
west
south
take space law space brochure
north
north
east
north
west
west
take whirled peas
east
east
south
south
east
east
south
take hypercube
south
south
"""


ITEMS = [
    "weather machine",
    "shell",
    "candy cane",
    "whirled peas",
    "hypercube",
    "space law space brochure",
    "space heater",
    "spool of cat6"
]


# Scientifically derived
WINNING_COMMANDS = """
south
west
south
take shell
north
north
take weather machine
west
south
east
take candy cane
west
north
east
south
east
east
south
take hypercube
south
south
east
"""


drop = lambda x: list(map(ord, "drop " + x + "\n"))


# This was the 'scientific derivation'
def guesses_lol(ins):
    # Get to the state right before the pressure plate with all items
    comp = Intcode(ins, inputs=list(map(ord, COLLECT_ALL_ITEMS)))
    comp.run()
    # Save this state
    pc = comp.pc
    rel_base = comp.rel_base

    for dropped in range(1, len(ITEMS)):
        for comb in combinations(ITEMS, dropped):
            comp = Intcode(ins[:])
            comp.pc = pc
            comp.rel_base = rel_base

            for item in comb:
                for c in drop(item):
                    comp.inputs.put(c)

            for c in map(ord, "east\n"):
                comp.inputs.put(c)

            print("Running with dropped ", comb)
            if comp.run(print_last=True):
                return


def sol1(ins):
    guesses_lol(ins[:])
    comp = Intcode(ins[:], inputs=list(map(ord, WINNING_COMMANDS)))
    comp.run(interactive=True, print_last=True)


def main():
    with open(sys.argv[1]) as f:
        ins = list(map(int, f.read().split(',')))

    sol1(ins)


if __name__ == '__main__':
    main()