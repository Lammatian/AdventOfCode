#include <iostream>
#include <optional>

#include "util.h"

using namespace std;
using namespace util;

pair<ll, ll> parse_nums(const string& s) {
    vector<string> colon_split = split(s, ": ");
    vector<string> comma_split = split(colon_split[1], ", ");
    return {stoll(comma_split[0].substr(2)), stoll(comma_split[1].substr(2))};
}

optional<pair<ll, ll>> solve(ll x1, ll y1, ll x2, ll y2, ll p1, ll p2) {
    ll r1_num = p2 * x2 - p1 * y2;
    ll r1_denom = x2 * y1 - y2 * x1;
    if (!(r1_num % r1_denom == 0)) return nullopt;
    ll r2_num = p1 * y1 - p2 * x1;
    ll r2_denom = x2 * y1 - y2 * x1;
    if (!(r2_num % r2_denom == 0)) return nullopt;
    return pair<ll, ll>{r1_num / r1_denom, r2_num / r2_denom};
}

int main() {
    string line;
    ll result1 = 0;
    ll result2 = 0;
    while (getline(cin, line)) {
        auto [x1, y1] = parse_nums(line); 
        getline(cin, line);
        auto [x2, y2] = parse_nums(line);
        getline(cin, line);
        auto [p1, p2] = parse_nums(line);
        if (auto sol = solve(x1, y1, x2, y2, p1, p2)) {
            result1 += 3 * sol->first + sol->second;
        }
        if (auto sol = solve(x1, y1, x2, y2, 10'000'000'000'000 + p1, 10'000'000'000'000 + p2)) {
            result2 += 3 * sol->first + sol->second;
        }
        getline(cin, line);
    }

    cout << result1 << "\n";
    cout << result2 << "\n";

    return 0;
}
