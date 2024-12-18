#include <iostream>
#include <queue>
#include <set>

#include "util.h"

using namespace std;
using namespace util;

struct cost_pos {
    ll cost;
    pos p;

    friend bool operator<(const cost_pos& cp1, const cost_pos& cp2) {
        return cp1.cost > cp2.cost;
    }
};

ll shortest_path(const board<char>& b, pos start, pos end) {
    priority_queue<cost_pos> q;
    set<pos> visited;
    q.push({0, start});
    visited.insert(start);
    while (!q.empty()) {
        auto [cost, curr] = q.top();
        q.pop();
        if (curr == end) return cost;
        for (auto n: b.neighbours(curr)) {
            if (b[n] == '#' || visited.find(n) != visited.end()) continue;
            q.push({cost + 1, n});
            visited.insert(n);
        }
    }

    return -1;
}

int main(int argc, char** argv) {
    string line;
    vector<pos> obstacles;
    int R = 0;
    int C = 0;
    int startbytes = stoi(argv[1]);
    while (cin >> line) {
        auto s = split(line, ",");
        auto c = stoi(s[0]);
        auto r = stoi(s[1]);
        R = max(R, r + 1);
        C = max(C, c + 1);
        obstacles.push_back({r, c});
    }

    board<char> b(R, C, '.');
    for (int i = 0; i < startbytes; ++i) {
        b[obstacles[i]] = '#';
    }

    pos start = {0, 0};
    pos end = {b.maxr - 1, b.maxc - 1};
    cout << shortest_path(b, start, end) << "\n";
    // IDEA: To optimise, can check if new byte is on the last path - only then recalculate
    for (int i = startbytes; i < obstacles.size(); ++i) {
        b[obstacles[i]] = '#';
        if (shortest_path(b, start, end) == -1) {
            cout << obstacles[i].c << "," << obstacles[i].r << "\n";
            break;
        }
    }

    return 0;
}
