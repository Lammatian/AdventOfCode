#include <iostream>
#include <fstream>
#include <map>
#include <cassert>

#include "util.h"

using namespace std;

ll SUBJECT_NUMBER = 7;
ll MOD = 20201227;

// Find an exponent such that base^exp = power (mod m)
// Very inefficient
ll dlog(ll base, ll power, ll m) {
    ll result = 0;
    ll cur = 1;

    while (cur != power) {
        result++;
        cur = cur * base % m;
    }

    return result;
}

// Calculate base^exp (mod m)
ll modexp(ll base, ll exp, ll m) {
    map<ll, ll> memo;
    // base^1 = base
    memo[1] = base;

    ll n = 2;

    while (n <= exp) {
        // base^n = (base^(n/2))^2, for all n = 2^k
        memo[n] = memo[n/2] * memo[n/2] % m;
        n *= 2;
    }

    ll result = 1;
    n = 1;

    while (exp > 0) {
        if (exp % 2 == 1) {
            result = result * memo[n] % m;
        }

        n *= 2;
        exp /= 2;
    }
    
    return result;
}

tuple<ll, ll> prep(const vector<string>& lines) {
    return make_tuple(stoll(lines[0]), stoll(lines[1]));
}

ll sol1(ll first, ll second) {
    ll loop_size1 = dlog(SUBJECT_NUMBER, first, MOD);
    ll loop_size2 = dlog(SUBJECT_NUMBER, second, MOD);
    assert(modexp(second, loop_size1, MOD) == modexp(first, loop_size2, MOD));
    return modexp(second, loop_size1, MOD);
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);
    ll first, second;
    tie(first, second) = prep(util::readlines(f));

    cout << sol1(first, second) << endl;

    return 0;
}
