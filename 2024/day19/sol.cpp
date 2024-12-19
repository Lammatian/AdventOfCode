#include <iostream>
#include <map>

#include "util.h"

using namespace std;
using namespace util;

map<pair<string, size_t>, ll> mem;

ll arrangements(const string& design, const vector<string>& patterns, size_t idx) {
    if (idx == design.size()) return 1LL;
    if (mem.find({design, idx}) != mem.end()) return mem[{design, idx}];

    ll result = 0LL;
    for (auto& p: patterns) {
        if (design.substr(idx, p.size()) == p) {
            result = result + arrangements(design, patterns, idx + p.size());
        }
    }

    mem[{design, idx}] = result;
    return result;
}

int main() {
    string line;
    getline(cin, line);
    vector<string> patterns = split(line, ", ");
    vector<string> designs;

    while (cin >> line) {
        designs.push_back(line);
    }

    cout << accumulate(designs.begin(), designs.end(), 0,
                [&patterns](int acc, const string& d) { return acc + (arrangements(d, patterns, 0) > 0); }) << "\n";
    cout << accumulate(designs.begin(), designs.end(), 0LL,
                [&patterns](ll acc, const string& d) { return acc + arrangements(d, patterns, 0); }) << "\n";

    return 0;
}
