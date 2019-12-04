def sol1(modules):
    return sum([x//3 - 2 for x in modules])


def sol2(modules):
    return sum([fuel_for_module(x) for x in modules])


def fuel_for_module(module):
    result = 0

    while module > 8:
        module = module//3 - 2
        result += module

    return result
    
def main():
    with open('input0.txt') as f:
        modules = list(map(int, f.readlines()))

    print(sol1(modules))
    print(sol2(modules))


if __name__ == '__main__':
    main()