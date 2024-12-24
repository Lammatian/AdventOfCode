#include <iostream>
#include <print>
#include <functional>
#include <queue>

#include "util.h"

using namespace std;
using namespace util;

enum class Op {
    AND, OR, XOR
};

struct gate {
    string in1;
    string in2;
    function<ll(ll, ll)> f;
    string out;
    string op;
    bool solved = false;

    friend ll apply_gate(map<string, ll>& wires, gate& g) {
        if (wires.find(g.in1) == wires.end() || wires.find(g.in2) == wires.end()) return -1;
        g.solved = true;
        return g.f(wires[g.in1], wires[g.in2]);
    }

    friend ostream& operator<<(ostream& o, const gate& g) {
        return o << g.in1 << " " << g.op << " " << g.in2 << " -> " << g.out;
    }
};

vector<string> connections(const vector<gate>& gates, const string& w) {
    vector<string> result = {w};
    set<string> seen = {w};
    queue<string> q;
    for (auto& g: gates) {
        if (g.in1 == w or g.in2 == w) {
            q.push(g.out);
            seen.insert(g.out);
        }
    }

    while (!q.empty()) {
        string curr = q.front(); q.pop();
        result.push_back(curr);
        for (auto& g: gates) {
            if (seen.find(g.out) != seen.end()) continue;
            if (g.in1 == curr or g.in2 == curr) {
                q.push(g.out);
                seen.insert(g.out);
            }
        }
    }

    return result;
}

ll wire_val(const vector<string>& wire_names, map<string, ll> wires, char prefix) {
    ll result = 0;
    for (auto& w: wire_names) {
        if (w.front() == prefix) result = 2 * result + wires[w];
    }

    return result;
}

string bin(ll v) {
    return std::bitset<64>(v).to_string();
}

int main() {
    map<string, function<ll(ll, ll)>> op2fun = {
        {"AND", [](ll a, ll b) { return a & b; }},
        {"OR", [](ll a, ll b) { return a | b; }},
        {"XOR", [](ll a, ll b) { return a ^ b; }}
    };
    map<string, ll> wires;
    map<string, vector<string>> wire_to_ops;
    map<string, string> wire_out_to_op;
    vector<gate> gates;
    string line;
    bool reading_wires = true;
    while (getline(cin, line)) {
        if (line == "") {
            reading_wires = false;
            continue;
        }
        if (reading_wires) {
            auto v = split(line, ": ");
            wires[v[0]] = stoll(v[1]);
        } else {
            auto s1 = split(line, " -> ");
            auto s2 = split(s1[0], " ");
            gates.push_back({s2[0], s2[2], op2fun[s2[1]], s1[1], s2[1]});
            wire_to_ops[s2[0]].push_back(s2[1]);
            wire_to_ops[s2[2]].push_back(s2[1]);
            wire_out_to_op[s1[1]] = s2[1];
        }
    }

    ll solved = 0;
    while (solved < gates.size()) {
        for (auto& g: gates) {
            if (g.solved) continue;
            ll app = apply_gate(wires, g);
            if (app >= 0) {
                wires[g.out] = app;
                solved++;
            }
        }
    }

    vector<string> wire_names;
    for (auto& [k, v]: wires) {
        wire_names.push_back(k);
    }
    sort(wire_names.begin(), wire_names.end(), greater<>());
    ll result = 0;
    for (auto& w: wire_names) {
        if (w[0] != 'z') break;
        result = 2 * result + wires[w];
    }
    println("{}", result);
    vector<string> result2 = {"z07", "z20", "z28", "vmv", "kfm", "hnv", "tqr", "hth"};
    sort(result2.begin(), result2.end());
    println("{}", join(result2, ","));

    for (auto& g: gates) {
        // Part 2 was mostly manual work:
        // 1. Figured out that 3 z's are not output of XOR but should be
        // 2. Figured out what is wrong around those z's by going from the previous z
        // 3. Applied changes and tried to sum the numbers again
        // 4. Found the last issue
        // z07 <> vmv
        // z20 <> kfm
        // z28 <> hnv
        // tqr <> hth
        if (g.out == "z07") g.out = "vmv";
        else if (g.out == "vmv") g.out = "z07";
        else if (g.out == "z20") g.out = "kfm";
        else if (g.out == "kfm") g.out = "z20";
        else if (g.out == "z28") g.out = "hnv";
        else if (g.out == "hnv") g.out = "z28";
        else if (g.out == "tqr") g.out = "hth";
        else if (g.out == "hth") g.out = "tqr";
        g.solved = false;
    }

    solved = 0;
    while (solved < gates.size()) {
        for (auto& g: gates) {
            if (g.solved) continue;
            ll app = apply_gate(wires, g);
            if (app >= 0) {
                wires[g.out] = app;
                solved++;
            }
        }
    }
    ll x = wire_val(wire_names, wires, 'x');
    ll y = wire_val(wire_names, wires, 'y');
    ll z = wire_val(wire_names, wires, 'z');
    println("x: {}", bin(x));
    println("y: {}", bin(y));
    println("z: {}", bin(z));
    println("x = {}, y = {}, x + y = {}, z = {}", x, y, x + y, z);

    return 0;
}
