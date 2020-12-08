#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <algorithm>

#include "util.h"

enum class Op {
    Acc,
    Nop,
    Jmp
};

std::map<std::string, Op> s2o = {
    {"acc", Op::Acc},
    {"nop", Op::Nop},
    {"jmp", Op::Jmp}
};

struct instruction {
    Op op;
    ll value;
};

class Console {
  public:
    ll global_state;
    std::vector<instruction> instructions;
    ll pc;

    Console(std::vector<instruction> instructions) :
      instructions{instructions} {
        pc = 0;
        global_state = 0;
    }

    void execute() {
        switch (instructions[pc].op) {
            case Op::Acc:
                global_state += instructions[pc].value;
                pc++;
                break;
            case Op::Nop:
                pc++;
                break;
            case Op::Jmp:
                pc += instructions[pc].value;
                break;
        }
    }

    instruction current() {
        return instructions[pc];
    }
};

std::vector<instruction> prep(std::vector<std::string> lines) {
    std::vector<instruction> result;

    for (auto& line: lines) {
        std::vector<std::string> tokens = util::tokenize(line, {' '});
        result.push_back({s2o[tokens[0]], std::stoll(tokens[1])});
    }

    return result;
}

ll sol1(const std::vector<instruction>& instructions) {
    std::set<ll> visited_pcs;
    Console console(instructions);

    while (visited_pcs.find(console.pc) == visited_pcs.end()) {
        visited_pcs.emplace(console.pc);
        console.execute();
    }

    return console.global_state;
}

std::set<ll> find_inloop_pcs(const std::vector<instruction>& instructions) {
    std::set<ll> visited_pcs;
    Console console(instructions);

    while (visited_pcs.find(console.pc) == visited_pcs.end()) {
        visited_pcs.emplace(console.pc);
        console.execute();
    }

    return visited_pcs;
}

bool ends_in_loop(ll start_pc,
                  const std::vector<instruction>& instructions,
                  const std::set<ll>& looping_pcs) {
    Console console(instructions);
    console.pc = start_pc;

    while (console.pc < instructions.size()) {
        if (looping_pcs.find(console.pc) != looping_pcs.end()) {
            return true;
        }

        console.execute();
    }

    return false;
}

std::set<ll> find_nonlooping_pcs(const std::vector<instruction>& instructions,
                                 const std::set<ll>& inloop_pcs) {
    std::set<ll> not_looping_pcs;

    for (int i = 0; i < instructions.size(); ++i) {
        if (!ends_in_loop(i, instructions, inloop_pcs)) {
            not_looping_pcs.emplace(i);
        }
    }

    return not_looping_pcs;
}

/**
 * Let P be the set of all the pcs (program counters)
 * 1. First, get the pcs that are in the loop (call this set L)
 * 2. Then, get the pcs that do not end up in that loop (N = subset of P - L)
 * 3. Then, for nops/jmps in the loop, see if swapping goes to a pc from N
 * If it does, then the swap will result in a non-looping boot -> good!
 * 4. Run the new set of instructions with this swap and return global state
 */
ll sol2(std::vector<instruction> instructions) {
    std::set<ll> visited_nop_pcs;
    std::set<ll> visited_jmp_pcs;
    Console console(instructions);

    // 1. Calculating P
    std::set<ll> visited_pcs = find_inloop_pcs(instructions);
    std::set<ll> visited_nop_jmp_pcs;
    std::for_each(visited_pcs.begin(),
                    visited_pcs.end(),
                    [&visited_nop_jmp_pcs, &instructions](ll pc) {
                        if (instructions[pc].op == Op::Nop || instructions[pc].op == Op::Jmp) {
                            visited_nop_jmp_pcs.emplace(pc);
                        }
                    });

    // 2. Calculating  N
    std::set<ll> not_looping_pcs = find_nonlooping_pcs(instructions, visited_pcs);

    // 3. Checking nop->jmp and jmp->nop swaps
    std::set<ll> pcs_to_swap;

    for (auto& pc: visited_nop_jmp_pcs) {
        instruction ins = instructions[pc];
        ll potential_new_pc = pc + (ins.op == Op::Nop ? ins.value : 1);
        if (not_looping_pcs.find(potential_new_pc) != not_looping_pcs.end()) {
            pcs_to_swap.emplace(pc);
        }
    }

    if (pcs_to_swap.empty()) {
        std::cerr << "Could not find a pc to swap" << std::endl;
        return -1;
    }

    // Swap first pc that makes it not loop (ideally should only be one such pc)
    ll swap_pc = *pcs_to_swap.begin();

    // 4a. Swap the instruction that was found
    if (instructions[swap_pc].op == Op::Nop) {
        instructions[swap_pc].op = Op::Jmp;
    } else {
        instructions[swap_pc].op = Op::Nop;
    }

    // 4b. Run the repaired console
    Console repaired_console(instructions);

    while (repaired_console.pc < instructions.size()) {
        repaired_console.execute();
    }

    return repaired_console.global_state;
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<instruction> instructions = prep(util::readlines(f));

    std::cout << sol1(instructions) << std::endl;
    std::cout << sol2(instructions) << std::endl;

    return 0;
}