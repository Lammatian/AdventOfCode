from enum import Enum
from typing import List
from queue import Queue


class Op(Enum):
    ADD = 1
    MUL = 2
    INP = 3
    OUT = 4
    JIT = 5
    JIF = 6
    LT = 7
    EQ = 8
    URB = 9
    END = 99


class PMode(Enum):
    ABS = 0
    IMM = 1
    REL = 2


class Parameter:
    def __init__(self, value: int, mode: PMode) -> None:
        self.value = value
        self.mode = mode

    def __repr__(self) -> str:
        return '({}, {})'.format(str(self.value), self.mode.name)


c2p = {
    Op.ADD: 3,
    Op.MUL: 3,
    Op.INP: 1,
    Op.OUT: 1,
    Op.JIT: 2,
    Op.JIF: 2,
    Op.LT: 3,
    Op.EQ: 3,
    Op.URB: 1,
    Op.END: 0
}


class Command:
    def __init__(self, pc: int, ins: List[int]) -> None:
        opcode = ins[pc]
        self.params: List[Parameter] = []

        self.op = Op(opcode % 100)
        opcode //= 100

        for i in range(c2p[self.op]):
            self.params.append(Parameter(ins[pc + i + 1], PMode(opcode%10)))
            opcode //= 10

    def __repr__(self) -> str:
        return '{} {}'.format(self.op.name, repr(self.params))


class Status(Enum):
    END = 0
    RUN = 1


class Intcode:
    def __init__(self, instructions: List[int], inputs: List[int] = None) -> None:
        self.ins = instructions
        self.ins.extend([0] * 1000)
        self.pc = 0
        self.rel_base = 0
        
        self.inputs = Queue()
        self.outputs = []

        if inputs:
            for i in inputs:
                self.inputs.put(i)

    def _get_parameter_value(self, parameter: PMode, is_write: bool = False) -> int:
        if not is_write:
            if parameter.mode == PMode.ABS:
                return self.ins[parameter.value]
            elif parameter.mode == PMode.IMM:
                return parameter.value
            elif parameter.mode == PMode.REL:
                return self.ins[parameter.value + self.rel_base]
        else:
            if parameter.mode == PMode.ABS:
                return parameter.value
            elif parameter.mode == PMode.REL:
                return parameter.value + self.rel_base

    def execute(self, wait_input: bool = True, in_val: int = -1, debug: bool = False) -> Status:
        command = Command(self.pc, self.ins)
        if debug:
            print(command)

        if command.op == Op.ADD:
            t1 = self._get_parameter_value(command.params[0])
            t2 = self._get_parameter_value(command.params[1])
            t3 = self._get_parameter_value(command.params[2], True)
            self.ins[t3] = t1 + t2
            self.pc += len(command.params) + 1
        elif command.op == Op.MUL:
            t1 = self._get_parameter_value(command.params[0])
            t2 = self._get_parameter_value(command.params[1])
            t3 = self._get_parameter_value(command.params[2], True)
            self.ins[t3] = t1 * t2
            self.pc += len(command.params) + 1
        elif command.op == Op.INP:
            t1 = self._get_parameter_value(command.params[0], True)
            if not self.inputs.empty() or wait_input:
                self.ins[t1] = self.inputs.get()
            else:
                self.ins[t1] = in_val
            self.pc += len(command.params) + 1
        elif command.op == Op.OUT:
            t1 = self._get_parameter_value(command.params[0])
            self.outputs.append(t1)
            self.pc += len(command.params) + 1
        elif command.op == Op.JIT:
            t1 = self._get_parameter_value(command.params[0])
            if t1:
                self.pc = self._get_parameter_value(command.params[1])
            else:
                self.pc += len(command.params) + 1
        elif command.op == Op.JIF:
            t1 = self._get_parameter_value(command.params[0])
            if not t1:
                self.pc = self._get_parameter_value(command.params[1])
            else:
                self.pc += len(command.params) + 1
        elif command.op == Op.LT:
            t1 = self._get_parameter_value(command.params[0])
            t2 = self._get_parameter_value(command.params[1])
            t3 = self._get_parameter_value(command.params[2], True)
            if t1 < t2:
                self.ins[t3] = 1
            else:
                self.ins[t3] = 0
            self.pc += len(command.params) + 1
        elif command.op == Op.EQ:
            t1 = self._get_parameter_value(command.params[0])
            t2 = self._get_parameter_value(command.params[1])
            t3 = self._get_parameter_value(command.params[2], True)
            if t1 == t2:
                self.ins[t3] = 1
            else:
                self.ins[t3] = 0
            self.pc += len(command.params) + 1
        elif command.op == Op.URB:
            t1 = self._get_parameter_value(command.params[0])
            self.rel_base += t1
            self.pc += len(command.params) + 1
        elif command.op == Op.END:
            return Status.END

        return Status.RUN

    # Returns True if run finished naturally, False otherwise
    def run(self, debug=False, interactive=False, print_out=False, print_last=False) -> bool:
        command_ord = list(map(ord, "Command?\n"))
        last_out = []

        while self.execute(debug=debug) != Status.END:
            outs = self.outputs

            if outs and outs[-1] == ord('\n'):
                if print_out:
                    print(''.join(map(chr, self.outputs)), end='')

                last_out += outs
                self.outputs = []

            if outs == command_ord and self.inputs.empty():
                if not interactive:
                    if print_last:
                        print('NO INPUTS, STOPPING RUN')

                    return False
                else:
                    command = list(map(ord, input('Input: '))) + [ord('\n')]

                    for c in command:
                        self.inputs.put(c)
                
            if outs == command_ord:
                last_out = []
            
        if print_last:
            print(''.join(map(chr, last_out)))

        return True
