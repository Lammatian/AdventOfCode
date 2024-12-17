#include <iostream>
#include <algorithm>
#include <random>

#include "util.h"

using namespace std;
using namespace util;

struct exe {
    ll A;
    ll B;
    ll C;
    int ip = 0;
    vector<int> program;
    vector<int> output;

    void simulate() {
        while (ip < program.size()) next();
    }

    void next() {
        auto opcode = program[ip];
        if (opcode == 0) adv();
        if (opcode == 1) bxl();
        if (opcode == 2) bst();
        if (opcode == 3) jnz();
        if (opcode == 4) bxc();
        if (opcode == 5) out();
        if (opcode == 6) bdv();
        if (opcode == 7) cdv();

        if (opcode != 3) ip += 2;
    }

    ll combo() {
        if (program[ip + 1] < 4) return program[ip + 1];
        if (program[ip + 1] == 4) return A;
        if (program[ip + 1] == 5) return B;
        if (program[ip + 1] == 6) return C;
        throw "Invalid combo parameter";
    }

    ll lit() {
        return program[ip + 1];
    }

    // The adv instruction (opcode 0) performs division. The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The result of the division operation is truncated to an integer and then written to the A register.
    void adv() {
        A /= (1 << combo());
    }

    // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand, then stores the result in register B.
    void bxl() {
        B ^= lit();
    }

    // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.
    void bst() {
        B = combo() % 8;
    }

    // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
    void jnz() {
        if (A != 0) ip = lit();
        else ip += 2;
    }

    // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
    void bxc() {
        B ^= C;
    }

    // The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If a program outputs multiple values, they are separated by commas.)
    void out() {
        output.push_back(combo() % 8);
    }

    // The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B register. (The numerator is still read from the A register.)
    void bdv() {
        B = A / (1 << combo());
    }

    // The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C register. (The numerator is still read from the A register.)
    void cdv() {
        C = A / (1 << combo());
    }

    friend ostream& operator<<(ostream& o, const exe& e) {
        o << "A: " << e.A << "\n";
        o << "B: " << e.B << "\n";
        o << "C: " << e.C << "\n";
        o << "ip: " << e.ip << "\n";
        o << e.program;
        return o;
    }
};

ll read_register() {
    ll reg;
    string token;
    cin >> token >> token >> reg;
    return reg;
}

string join_ints(const vector<int>& v) {
    return accumulate(v.begin(), v.end(), ""s,
                [](string acc, int x) {
                    return acc.empty() ? to_string(x) : acc + "," + to_string(x);
                });
}

ll next_val(ll curr, ll added) {
    ll val = (curr << 3) + added;
    ll b0 = val % 8;
    return ((b0 ^ 6) ^ (val >> (b0 ^ 3))) % 8;
}

vector<ll> solve(const exe& e, ll curr, size_t idx) {
    if (idx == -1) return {curr};
    vector<ll> possible_next;
    for (int i = 0; i < 8; ++i) {
        if (next_val(curr, i) == e.program[idx]) {
            possible_next.push_back((curr << 3) + i);
        }
    }

    if (possible_next.empty()) return {};
    vector<ll> solutions;
    for (auto x: possible_next) {
         auto sol = solve(e, x, idx - 1);
         for (auto s: sol) {
            solutions.push_back(s);
         }
    }

    return solutions;
}

int main() {
    exe e;
    string token;
    e.A = read_register();
    e.B = read_register();
    e.C = read_register();
    cin >> token;
    cin >> token;
    for (auto s: split(token, ",")) {
        e.program.push_back(stoi(s));
    }

    e.simulate();
    cout << join_ints(e.output) << "\n";

    // For reference: the solution that got me a star was actually trying random
    // shuffles of [0..7] and selecting the first number that works instead of
    // considering all the possible combinations
    vector<ll> quines = solve(e, 0, e.program.size() - 1);
    cout << accumulate(quines.begin(), quines.end(), quines.front(),
                [](ll acc, ll x) { return min(acc, x); }) << "\n";
    return 0;
}
