import sys
sys.path.append('..')
from util.intcode import Intcode


def sol1(ins):
    comps = {i: Intcode(ins[:], inputs=[i]) for i in range(50)}

    while True:
        for i, comp in comps.items():
            comp.execute(wait_input=False, debug=False)

            if len(comp.outputs) == 3:
                dest, x, y = comp.outputs

                if dest == 255:
                    return y

                comp.outputs = []
                comps[dest].inputs.put(x)
                comps[dest].inputs.put(y)


def sol2(ins):
    NAT = (0, 0)

    comps = {i: Intcode(ins[:], inputs=[i]) for i in range(50)}
    no_sent = 0
    delivered_ys = set()

    while True:
        for i, comp in comps.items():
            status = comp.execute(wait_input=False, debug=False)

            if len(comp.outputs) == 3:
                no_sent = 0
                dest, x, y = comp.outputs
                comp.outputs = []

                if dest == 255:
                    NAT = (x, y)
                    continue

                comps[dest].inputs.put(x)
                comps[dest].inputs.put(y)

        no_sent += 1

        # We assume that all servers are waiting if no packet was sent in 700
        # executions. This is super slow (~30-ish seconds)
        if no_sent == 700:
            x, y = NAT
            print("NAT SENDING", x, y, "TO", 0)

            if y in delivered_ys:
                return y
            
            delivered_ys.add(y)
            comps[0].inputs.put(x)
            comps[0].inputs.put(y)

    return 0


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        ins = list(map(int, f.read().strip().split(','))) 

    print(sol1(ins[:]))
    print(sol2(ins[:]))
