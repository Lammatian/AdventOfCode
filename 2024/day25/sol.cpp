#include <iostream>
#include <print>

#include <util.h>

using namespace std;
using namespace util;

int main() {
    vector<vector<int>> keys;
    vector<vector<int>> locks;
    string line;
    getline(cin, line, static_cast<char>(EOF));
    auto patterns = split(line, "\n\n");
    for (auto& p: patterns) {
        if (p[0] == '#') {
            keys.push_back(vector<int>(5, -1));
        } else {
            locks.push_back(vector<int>(5, -1));
        }
        vector<int>& curr = (p[0] == '#') ? keys.back() : locks.back();

        for (auto& r: split(p, "\n")) {
            for (int i = 0; i < 5; ++i) {
                if (r[i] == '#') {
                    curr[i]++;
                }
            }
        }
    }

    ll result = 0;
    for (auto& k: keys) {
        for (auto& l: locks) {
            bool valid = true;
            for (int i = 0; i < 5; ++i) {
                if (k[i] + l[i] > 5) valid = false;
            }
            if (valid) result++;
        }
    }
    println("{}", result);

    return 0;
}
