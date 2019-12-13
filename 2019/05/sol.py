import sys


def digit(n, i):
    """
    Get i-th digit of n
    """
    if i >= len(str(n)):
        return 0

    return int(str(n)[-(i + 1)])


def get_val(ins, pos, mode):
    """
    Get argument value based on the mode
    If mode = 0, gets ins[ins[pos]]
    If mode = 1, gets ins[pos]
    """
    if mode == 0:
        return ins[ins[pos]]
    if mode == 1:
        return ins[pos]


def parse_command(ins, cur, sol):
    """
    Parse command and return next position
    If no next position, return -1
    """
    op_code = ins[cur]
    op = op_code % 10
    modes = [digit(op_code, i) for i in range(2, 4)]

    if op < 3: # add/mul
        a1 = get_val(ins, cur + 1, modes[0])
        a2 = get_val(ins, cur + 2, modes[1])
        a3 = get_val(ins, cur + 3, 1)
        result = a1 + a2 if op == 1 else a1 * a2
        ins[a3] = result
        return cur + 4
    elif op == 3: # input
        a1 = get_val(ins, cur + 1, 1)
        ins[a1] = inp[sol]
        return cur + 2
    elif op == 4: # output
        a1 = get_val(ins, cur + 1, modes[0])
        print('Output:', a1)
        return cur + 2
    elif op == 5: # jump if true
        a1 = get_val(ins, cur + 1, modes[0])
        a2 = get_val(ins, cur + 2, modes[1])

        if a1 != 0:
            return a2
        
        return cur + 3
    elif op == 6: # jump if false
        a1 = get_val(ins, cur + 1, modes[0])
        a2 = get_val(ins, cur + 2, modes[1])

        if a1 == 0:
            return a2
        
        return cur + 3
    elif op == 7: # set less than
        a1 = get_val(ins, cur + 1, modes[0])
        a2 = get_val(ins, cur + 2, modes[1])
        a3 = get_val(ins, cur + 3, 1)

        if a1 < a2:
            ins[a3] = 1
        else:
            ins[a3] = 0

        return cur + 4
    elif op == 8: # set equals
        a1 = get_val(ins, cur + 1, modes[0])
        a2 = get_val(ins, cur + 2, modes[1])
        a3 = get_val(ins, cur + 3, 1)

        if a1 == a2:
            ins[a3] = 1
        else:
            ins[a3] = 0

        return cur + 4
    else: # 99 = end
        return -1


def sol1(ins):
    cur = 0

    while cur >= 0:
        cur = parse_command(ins, cur, 1)

    return ins[0]


def sol2(ins):
    cur = 0

    while cur >= 0:
        cur = parse_command(ins, cur, 2)

    return ins[0]


inp = {
    1: 1,
    2: 5
}


def main():
    with open(sys.argv[1]) as f:
        instructions = list(map(int, f.read().split(',')))

    print('First:', sol1(instructions[:]))
    print('Second:', sol2(instructions[:]))


if __name__ == '__main__':
    main()