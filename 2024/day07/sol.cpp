#include <iostream>
#include <vector>

#include "util.h"

using namespace std;
using ll = long long;

enum class Op { ADD, MUL, CAT };

struct eq {
    ll target;
    vector<ll> vals;

    eq(vector<string> s) {
        s[0].pop_back();
        this->target = stoll(s[0]);
        for (size_t i = 1; i < s.size(); ++i) {
            this->vals.push_back(stoll(s[i]));
        }
    }

    friend ostream& operator<<(ostream& o, eq e) {
        return o << "target: " << e.target << " | vals: " << e.vals;
    }
};

ll concat(ll a, ll b) {
    ll res = 1;
    while (res <= b) {
        res *= 10;
    }
    return a*res + b;
}

ll eval(const eq& e, ll result, size_t idx, Op op) {
    switch (op) {
        case Op::ADD:
            return result + e.vals[idx];
        case Op::MUL:
            return result * e.vals[idx];
        case Op::CAT:
            return concat(result, e.vals[idx]);
    }
}

bool acw(const eq& e, ll result, size_t idx, Op op, bool use_concat) {
    if (op == Op::CAT and !use_concat) return false;
    if (idx == e.vals.size()) return e.target == result;
    ll newres = eval(e, result, idx, op);
    return acw(e, newres, idx + 1, Op::ADD, use_concat)
        || acw(e, newres, idx + 1, Op::MUL, use_concat)
        || acw(e, newres, idx + 1, Op::CAT, use_concat);
}

bool any_combination_works(const eq& e, bool use_concat) {
    return acw(e, e.vals.front(), 1, Op::ADD, use_concat)
        || acw(e, e.vals.front(), 1, Op::MUL, use_concat)
        || acw(e, e.vals.front(), 1, Op::CAT, use_concat);
}

int main() {
    string line;
    ll p1 = 0;
    ll p2 = 0;
    while (getline(cin, line)) {
        eq e(util::split(line, " "));
        if (any_combination_works(e, false)) {
            p1 += e.target;
        }
        if (any_combination_works(e, true)) {
            p2 += e.target;
        }
    }

    cout << p1 << "\n";
    cout << p2 << "\n";

    return 0;
}
