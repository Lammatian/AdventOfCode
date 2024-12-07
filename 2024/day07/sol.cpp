#include <iostream>
#include <vector>

#include "util.h"

using namespace std;
using ll = long long;

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

// op 0 is +, 1 is *, 2 is ||
bool acw(const eq& e, ll res, size_t idx, int op, bool use_concat) {
    if (op == 2 and !use_concat) return false;
    if (idx == e.vals.size()) return res == e.target; 
    ll newres;
    if (op == 0) newres = res + e.vals[idx];
    else if (op == 1) newres = res * e.vals[idx];
    else if (op == 2) newres = concat(res, e.vals[idx]);
    return acw(e, newres, idx + 1, 0, use_concat) || acw(e, newres, idx + 1, 1, use_concat) || acw(e, newres, idx + 1, 2, use_concat);
}

bool any_combination_works(const eq& e, bool use_concat) {
    return acw(e, e.vals.front(), 1, 0, use_concat) || acw(e, e.vals.front(), 1, 1, use_concat) || acw(e, e.vals.front(), 1, 2, use_concat);
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
