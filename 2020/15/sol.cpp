#include <iostream>
#include <fstream>
#include <map>
#include <unordered_map>
#include <algorithm>

#include "util.h"

using namespace std;

vector<ll> prep(string line) {
    vector<string> nums_str = util::tokenize(line, ',');
    vector<ll> result(nums_str.size());
    transform(nums_str.begin(), nums_str.end(), result.begin(),
              [](string s){return stoll(s);});

    return result;
}

ll play_game(vector<ll> start, ll turns) {
    vector<ll> history(turns, -1);

    for (int i = 0; i < start.size() - 1; ++i) {
        history[start[i]] = i + 1;
    }

    ll last = start.back();

    for (int turn = start.size() + 1; turn <= turns; ++turn) {
        ll prev_last = last;

        if (history[last] != -1) {
            last = turn - 1 - history[last];
        } else {
            last = 0;
        }

        history[prev_last] = turn - 1;
    }

    return last;
}

ll sol1(vector<ll> nums) {
    return play_game(nums, 2020);
}

ll sol2(vector<ll> nums) {
    return play_game(nums, 30'000'000);
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    string line;
    getline(f, line);
    vector<ll> nums = prep(line);

    cout << sol1(nums) << endl;
    cout << sol2(nums) << endl;

    return 0;
}
