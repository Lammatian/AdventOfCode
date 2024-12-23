#include <iostream>
#include <print>
#include <vector>
#include <map>
#include <set>

#include "util.h"

using namespace std;
using namespace util;

struct three_conn {
    set<string> cs;

    three_conn(string a, string b, string c) {
        cs = {a, b, c};
    }

    friend bool operator<(const three_conn& t1, const three_conn& t2) {
        return t1.cs < t2.cs;
    }
};

/**
 * https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm, the easiest one
 * This implementation is atrocious too lol
 */
vector<set<string>> bron_kerbosch(map<string, set<string>>& v2e, set<string> R, set<string> P, set<string> X) {
    if (P.empty() && X.empty()) {
        return {R};
    }

    vector<set<string>> result;
    auto newP = P;
    for (auto& p: newP) {
        auto R2 = R;
        R2.insert(p);
        auto P2 = intersection(P, v2e[p]);
        auto X2 = intersection(X, v2e[p]);
        for (auto& c: bron_kerbosch(v2e, R2, P2, X2)) {
            result.push_back(c);
        }
        P.erase(p);
        X.insert(p);
    }

    return result;
}

int main() {
    map<string, set<string>> neighbours;
    set<pair<string, string>> edges;
    set<string> vertices;
    string line;
    set<string> t_computers;
    while (cin >> line) {
        string v1 = line.substr(0, 2);
        string v2 = line.substr(3, 2);
        neighbours[v1].insert(v2);
        neighbours[v2].insert(v1);
        edges.insert({v1, v2});
        edges.insert({v2, v1});
        vertices.insert(v1);
        vertices.insert(v2);
        if (v1[0] == 't') t_computers.insert(v1);
        if (v2[0] == 't') t_computers.insert(v2);
    }

    set<three_conn> three_conns;
    for (auto& t: t_computers) {
        for (auto& c1: neighbours[t]) {
            for (auto& c2: neighbours[t]) {
                if (c1 != c2 && edges.find({c1, c2}) != edges.end()) {
                    three_conns.insert({t, c1, c2});
                }
            }
        }
    }
    println("{}", three_conns.size());

    set<string> largest_clique;
    size_t largest_clique_size = 0;
    for (auto& c: bron_kerbosch(neighbours, {}, vertices, {})) {
        if (c.size() > largest_clique_size) {
            largest_clique = c;
            largest_clique_size = c.size();
        }
    }
    vector<string> clique(largest_clique.begin(), largest_clique.end());
    sort(clique.begin(), clique.end());
    println("{}",
            accumulate(
                clique.begin(),
                clique.end(),
                clique.front(),
                [&clique](string acc, string x) { return (x == clique.front()) ? acc : acc + "," + x; }
            ));

    return 0;
}
