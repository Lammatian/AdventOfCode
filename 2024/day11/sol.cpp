#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <numeric>

#include "util.h"

using namespace std;

map<pair<ll, ll>, ll> memo;

vector<ll> blink(ll stone) {
    if (stone == 0) return {1LL};
    string stone_str = to_string(stone);
    if (stone_str.size() % 2 == 0) {
        return {
            stoll(stone_str.substr(0, stone_str.size() / 2)),
            stoll(stone_str.substr(stone_str.size() / 2))
        };
    }
    return {2024LL * stone};
}

ll multiblink(ll init, ll steps) {
    if (steps == 0) return 1;
    if (memo.find({init, steps}) != memo.end()) {
        return memo[{init, steps}];
    }

    ll result = 0;
    for (auto s: blink(init)) {
        result += multiblink(s, steps - 1); 
    }
    memo[{init, steps}] = result;
    return result;
}

int main() {
    vector<ll> nums;
    ll num;
    while (cin >> num) nums.push_back(num);

    cout << accumulate(nums.begin(), nums.end(), 0, [](ll acc, ll num) {
        return acc + multiblink(num, 25);
    }) << "\n";

    cout << accumulate(nums.begin(), nums.end(), 0LL, [](ll acc, ll num) {
        return acc + multiblink(num, 75);
    }) << "\n";

    return 0;
}
