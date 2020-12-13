#include <iostream>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <tuple>

#include "util.h"

using namespace std;

struct bus {
    ll id;
    // part 2 only
    ll dtime;
};

typedef vector<bus> Buses;

pair<ll, Buses> parse(vector<string> lines) {
    ll time = stoi(lines[0]);
    vector<string> buses_str = util::tokenize(lines[1], ',');
    Buses buses;
    ll cur_dtime = -1;

    for (auto& bus: buses_str) {
        cur_dtime++;
        if (bus == "x") {
            continue;
        }

        buses.push_back({stoll(bus), cur_dtime});
    }

    return {time, buses};
}

ll sol1(const pair<ll, Buses>& time_buses) {
    ll time = time_buses.first;
    Buses buses = time_buses.second;
    ll best_id = buses[0].id;
    ll best_dep = (best_id - time) % best_id;
    vector<pair<ll, ll>> lines_deps(buses.size());
    std::transform(buses.begin(), buses.end(), lines_deps.begin(),
                   [&time](bus busline) {
                       return pair<ll, ll>((busline.id - (time % busline.id)) % busline.id, busline.id);
                   });

    auto best_bus = *std::min_element(lines_deps.begin(), lines_deps.end());
    return best_bus.first * best_bus.second;
}

tuple<ll, ll, ll> egcd(ll a, ll b) {
    if (a == 0) {
        return make_tuple(b, 0, 1);
    }

    ll gcd, x, y;
    tie(gcd, x, y) = egcd(b % a, a);

    return make_tuple(gcd, (y - (b/a) * x), x);
}

ll modinv(ll a, ll b) {
    ll gcd, x, y;
    tie(gcd, x, y) = egcd(a, b);

    if (gcd != 1) {
        throw runtime_error("Modular inverse does not exist");
    }

    return (x % b + b) % b;
}

ll crt(vector<pair<ll, ll>> eqns) {
    vector<ll> moduli(eqns.size());
    transform(eqns.begin(), eqns.end(), moduli.begin(), [](auto eq){return eq.second;});
    ll M = accumulate(moduli.begin(), moduli.end(), 1LL, multiplies<ll>());
    ll result = 0;

    for (auto& eq: eqns) {
        ll bi = M/eq.second;
        result += eq.first * bi * modinv(bi, eq.second);
    }

    return (result % M + M) % M;
}

ll sol2(Buses buses) {
    vector<pair<ll, ll>> crt_eqns;

    for (int i = 1; i < buses.size(); ++i) {
        crt_eqns.push_back({
            -modinv(buses[0].id, buses[i].id) * buses[i].dtime % buses[i].id,
            buses[i].id
        });
    }

    return buses[0].id * crt(crt_eqns);
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    ll time;
    Buses buses;
    tie(time, buses) = parse(util::readlines(f));

    cout << sol1({time, buses}) << endl;
    cout << sol2(buses) << endl;

    return 0;
}