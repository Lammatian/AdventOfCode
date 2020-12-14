#include <iostream>
#include <fstream>
#include <map>
#include <regex>

#include "util.h"

using namespace std;

struct mask {
    ll and_mask;
    ll or_mask;
};

// And mask has 0s for 0s and 1s for 1s and Xs
ll get_and_mask(const string& line) {
    ll result = 0;

    for (auto it = line.begin(); it != line.end(); ++it) {
        if (*it == '0') {
            result *= 2;
        } else {
            result = 2*result + 1;
        }
    }

    return result;
}

// Or mask has 0s for 0s and Xs and 1s for 1s
ll get_or_mask(const string& line) {
    ll result = 0;

    for (auto it = line.begin(); it != line.end(); ++it) {
        if (*it == '1') {
            result = 2*result + 1;
        } else {
            result *= 2;
        }
    }

    return result;
}

// Floating mask has 0s for 0s, 1s for 1s and both 1/0 for Xs
vector<mask> get_floating_mask(const string& line) {
    vector<mask> result;
    result.push_back({0, 0});

    for (auto it = line.begin(); it != line.end(); ++it) {
        // 0 is 0 in OR and 1 in AND
        // 1 is 1 in both
        if (*it == '0') {
            for (auto& m: result) {
                m.and_mask = 2*m.and_mask + 1;
                m.or_mask = 2*m.or_mask;
            }
        } else if (*it == '1') {
            for (auto& m: result) {
                m.and_mask = 2*m.and_mask + 1;
                m.or_mask = 2*m.or_mask + 1;
            }
        } else {
            vector<mask> new_res;

            for (int i = 0; i < result.size(); ++i) {
                new_res.push_back({result[i].and_mask * 2 + 1, result[i].or_mask * 2 + 1});
                result[i].and_mask *= 2;
                result[i].or_mask *= 2;
            }

            result.insert(result.end(), new_res.begin(), new_res.end());
        }
    }

    return result;
}

ll sol1(const vector<string>& lines) {
    ll and_mask, or_mask;
    map<ll, ll> memory;
    regex mem_update_regex ("^.*\\[(\\d+)\\] = (\\d+)$");
    smatch matches;
    ll result = 0;

    for (auto& line: lines) {
        if (line[1] == 'a') {
            and_mask = get_and_mask(line.substr(7));
            or_mask = get_or_mask(line.substr(7));
        } else {
            regex_search(line, matches, mem_update_regex);
            ll value = stoll(matches[2]);
            value &= and_mask;
            value |= or_mask;
            result = result - memory[stoll(matches[1])] + value; 
            memory[stoll(matches[1])] = value;
        }
    }

    return result;
}

ll sol2(vector<string> lines) {
    vector<mask> masks;
    map<ll, ll> memory;
    regex mem_update_regex ("^.*\\[(\\d+)\\] = (\\d+)$");
    smatch matches;
    ll result = 0;

    for (auto& line: lines) {
        if (line[1] == 'a') {
            masks = get_floating_mask(line.substr(7));
        } else {
            regex_search(line, matches, mem_update_regex);
            ll addr = stoll(matches[1]);
            ll value = stoll(matches[2]);

            for (auto& mask: masks) {
                ll masked_addr = (addr & mask.and_mask ) | mask.or_mask;
                result = result - memory[masked_addr] + value; 
                memory[masked_addr] = value;
            }
        }
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    vector<string> lines = util::readlines(f);

    cout << sol1(lines) << endl;
    cout << sol2(lines) << endl;

    return 0;
}