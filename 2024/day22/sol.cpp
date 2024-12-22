#include <iostream>
#include <print>
#include <set>

#include "util.h"

using namespace std;
using namespace util;

ll next_secret(ll curr) {
    ll M = 1 << 24;
    ll step1 = ((curr << 6) ^ curr) % M;
    ll step2 = ((step1 >> 5) ^ step1) % M;
    ll step3 = ((step2 << 11) ^ step2) % M;
    return step3;
}

ll nth_secret(ll start, int n) {
    for (int i = 0; i < n; ++i) {
        start = next_secret(start);
    }
    return start;
}

map<vector<ll>, ll> changes_to_price(ll curr, int n) {
    map<vector<ll>, ll> result;
    vector<ll> diffs;
    for (int i = 0; i < 4; ++i) {
        ll next = next_secret(curr);
        ll diff = (next % 10) - (curr % 10);
        diffs.push_back(diff);
        curr = next;
    }

    result[diffs] = curr % 10;

    for (int i = 4; i < n; ++i) {
        ll next = next_secret(curr);
        diffs.push_back((next % 10) - (curr % 10));
        diffs.erase(diffs.begin());
        curr = next;
        if (result.find(diffs) == result.end()) result[diffs] = curr % 10;
    }

    return result;
}

int main() {
    ll result = 0;
    ll secret;
    map<vector<ll>, ll> best_change_prices;
    ll best = 0;
    while (cin >> secret) {
        result += nth_secret(secret, 2000);
        auto c2p = changes_to_price(secret, 2000);
        for (auto& [k, v]: c2p) {
            best_change_prices[k] += v;
            best = max(best, best_change_prices[k]);
        }
    }
    println("{}", result);
    println("{}", best);

    return 0;
}
