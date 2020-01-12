import sys
sys.path.append('..')
from util.intcode import Intcode

def sol1(data):
    """
    If any of A, B or C are holes and D is ground
    then jump
    """
    inp = """NOT A J
    NOT B T
    OR T J
    NOT C T
    OR T J
    AND D J
    WALK
    """
    ascii_input = list(map(ord, inp))

    computer = Intcode(data, ascii_input)
    computer.simulate()

    if computer.outputs[-1] > 255:
        return computer.outputs[-1]
    
    output = ''.join(list(map(chr, computer.outputs)))
    print(output)


def sol2(data):
    """
    If (any of A, B or C are holes and D is ground)
    and (H is ground or (E is ground and I is ground))
    or (A is hole and D is ground)
    then jump
    """
    inp = """NOT A J
    NOT B T
    OR T J
    NOT C T
    OR T J
    AND D J
    NOT E T
    NOT T T
    AND I T
    OR H T
    AND T J
    NOT A T
    AND D T
    OR T J
    RUN
    """
    ascii_input = list(map(ord, inp))

    computer = Intcode(data, ascii_input)
    computer.simulate()

    if computer.outputs[-1] > 255:
        return computer.outputs[-1]
    
    output = ''.join(list(map(chr, computer.outputs)))
    print(output)


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read().strip().split(',')))
    
    print(sol1(data))
    print(sol2(data))
    

if __name__ == '__main__':
    main()