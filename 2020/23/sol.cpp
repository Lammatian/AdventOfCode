#include <iostream>
#include <fstream>
#include <map>
#include <unordered_map>

#include "util.h"

using namespace std;

using Cups = vector<ll>;

string cup_sequence(Cups& cups, ll start) {
    string result = "";
    result += to_string(start);
    ll cur_val = cups[start];

    while (cur_val != start) {
        result += to_string(cur_val);
        cur_val = cups[cur_val];
    }

    return result;
}

tuple<Cups, ll> prep(const vector<string>& lines) {
    string start_pos = lines[0];
    int N = start_pos.size();
    Cups cups(N + 1);

    for (int i = 0; i < start_pos.size(); ++i) {
        cups[start_pos[i] - '0'] = start_pos[(i + 1) % N] - '0';
    }

    return make_tuple(cups, start_pos[0] - '0');
}

tuple<Cups, ll> big_prep(const vector<string>& lines) {
    string start_pos = lines[0];
    int N = start_pos.size();
    int BN = 1'000'000;
    Cups cups(BN + 1);

    for (int i = 0; i < start_pos.size(); ++i) {
        cups[start_pos[i] - '0'] = start_pos[(i + 1) % N] - '0';
    }

    // Fix last of the starting position
    cups[start_pos.back() - '0'] = N + 1;

    for (int i = start_pos.size() + 1; i <= BN; ++i) {
        cups[i] = i + 1;
    }

    // Fix last of the consecutive
    cups[BN] = start_pos.front() - '0';

    return make_tuple(cups, start_pos[0] - '0');
}

ll destination_value(Cups& cups, ll cur_val) {
    ll first = cups[cur_val];
    ll second = cups[first];
    ll third = cups[second];
    int N = cups.size() - 1;

    for (int i = 1; i < 5; ++i) {
        ll val = (cur_val - i - 1 + N) % N + 1;

        if (val != first && val != second && val != third) {
            return val;
        }
    }
}

string sol1(Cups& cups, ll start_val, int iters=100) {
    ll cur_val = start_val;
    int N = cups.size();

    for (int i = 0; i < iters; ++i) {
        ll first = cups[cur_val];
        ll second = cups[first];
        ll third = cups[second];
        ll dest_val = destination_value(cups, cur_val);

        cups[cur_val] = cups[third];
        ll temp = cups[dest_val];
        cups[dest_val] = first;
        cups[third] = temp;

        cur_val = cups[cur_val];
    }

    return cup_sequence(cups, 1).substr(1);
}

ll sol2(Cups& cups, ll start_val, int iters=10'000'000) {
    ll cur_val = start_val;
    int N = cups.size();

    for (int i = 0; i < iters; ++i) {
        ll first = cups[cur_val];
        ll second = cups[first];
        ll third = cups[second];
        ll dest_val = destination_value(cups, cur_val);

        cups[cur_val] = cups[third];
        ll temp = cups[dest_val];
        cups[dest_val] = first;
        cups[third] = temp;

        cur_val = cups[cur_val];
    }

    return cups[1] * cups[cups[1]];
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);
    auto lines = util::readlines(f);

    ll start_val;
    Cups cups;
    tie(cups, start_val) = prep(lines);

    cout << sol1(cups, start_val) << endl;

    ll big_start_val;
    Cups big_cups;
    tie(big_cups, big_start_val) = big_prep(lines);
    cout << sol2(big_cups, big_start_val) << endl;

    return 0;
}
