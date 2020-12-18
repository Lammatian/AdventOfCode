#include <iostream>
#include <fstream>
#include <cctype>
#include <sstream>

#include "util.h"

using namespace std;

enum class Op {
    ADD, MUL
};

class EqManager {
  public:
    Op current_op;
  
    EqManager(Op op) : current_op{op} {}

    void exec(ll& result, ll term) {
        if (current_op == Op::ADD) {
            result += term;
        } else {
            result *= term;
        }
    }

    void update_op(Op new_op) {
        current_op = new_op;
    }
};

ll eval(stringstream& s) {
    ll result = 0;
    EqManager eqm(Op::ADD);
    char curr;

    while (s >> curr) {
        if (curr == ' ') {
            continue;
        } else if (isdigit(curr)) {
            eqm.exec(result, curr - '0');
        } else if (curr == '(') {
            eqm.exec(result, eval(s));
        } else if (curr == ')') {
            return result;
        } else if (curr == '*') {
            eqm.update_op(Op::MUL);
        } else if (curr == '+') {
            eqm.update_op(Op::ADD);
        }
    }

    return result;
}

ll sol1(vector<string> eqns) {
    ll result = 0;

    for (auto& eqn: eqns) {
        stringstream seqn(eqn);
        result += eval(seqn);
    }

    return result;
}

ll eval2(stringstream& s) {
    ll result = 1;
    char curr;
    Op last_op = Op::MUL;
    // Sum between two consecutive multiplications
    ll current_sum = 0;

    while (s >> curr) {
        if (curr == ' ') {
            continue;
        } else if (isdigit(curr)) {
            current_sum += curr - '0';
        } else if (curr == '(') {
            current_sum += eval2(s);
        } else if (curr == ')') {
            return result * current_sum;
        } else if (curr == '*') {
            result *= current_sum;
            last_op = Op::MUL;
            current_sum = 0;
        } else if (curr == '+') {
            last_op = Op::ADD;
        }
    }

    return result * current_sum;
}

ll sol2(vector<string> eqns) {
    ll result = 0;

    for (auto& eqn: eqns) {
        stringstream seqn(eqn);
        result += eval2(seqn);
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    vector<string> eqns = util::readlines(f);

    cout << sol1(eqns) << endl;
    cout << sol2(eqns) << endl;

    return 0;
}