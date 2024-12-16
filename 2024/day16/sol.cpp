#include <iostream>
#include <queue>
#include <set>
#include <optional>
#include <math.h>

#include "util.h"

using namespace std;
using namespace util;

struct PathCost {
    ll priority;
    pair<pos, pos> loc;

    friend bool operator<(const PathCost& pc1, const PathCost& pc2) {
        return pc1.priority > pc2.priority;
    }

    friend ostream& operator<<(ostream& o, const PathCost& pc) {
        return o << pc.priority << " | " << pc.loc.first << ", " << pc.loc.second;
    }
};

struct ExtendedPathCost : public PathCost {
    set<pos> path;
};

ll best_path(board<char>& b, pos start) {
    priority_queue<PathCost> q;
    q.push({0, {start, {0, 1}}});
    set<pair<pos, pos>> visited;
    visited.insert({start, {0, 1}});
    while (!q.empty()) {
        auto [prio, cd] = q.top();
        pos curr = cd.first;
        pos dir = cd.second;
        q.pop();
        if (b[curr] == 'E') return prio;

        for (auto n: b.neighbours(curr)) {
            if (visited.find({n, n - curr}) != visited.end()) {
                continue;
            }
            if (b[n] != '#') {
                if (n - curr == dir) q.push({prio + 1, {n, dir}});
                else q.push({prio + 1001, {n, n - curr}});
                visited.insert({n, n - curr});
            }
        }
    }

    return -1;
}

set<pos> best_spots(board<char>& b, pos start, ll lowest_score) {
    set<pos> result;
    priority_queue<ExtendedPathCost> q;
    q.push({0, {start, {0, 1}}, {start}});
    map<pair<pos, pos>, pair<ll, set<pos>>> visited;
    visited[{start, {0, 1}}] = {0, {start}};
    pos end;
    while (!q.empty()) {
        auto epc = q.top();
        ll prio = epc.priority;
        pos curr = epc.loc.first;
        pos dir = epc.loc.second;
        set<pos> path = epc.path;
        q.pop();
        if (b[curr] == 'E') {
            end = curr;
            continue;
        }

        for (auto n: b.neighbours(curr)) {
            ll cost = (n - curr == dir) ? prio + 1 : prio + 1001;
            if (b[n] == '#') continue;
            if (visited.find({n, n - curr}) != visited.end()) {
                if (visited[{n, n - curr}].first == cost) {
                    visited[{n, n - curr}].second.insert(path.begin(), path.end());
                }
                continue;
            }
            if (b[n] != '#') {
                set<pos> new_path(visited[{curr, dir}].second.begin(), visited[{curr, dir}].second.end());
                new_path.insert(n);
                if (n - curr == dir) q.push({prio + 1, {n, dir}, new_path});
                else q.push({prio + 1001, {n, n - curr}, new_path});
                visited[{n, n - curr}] = {cost, new_path};
            }
        }
    }

    for (pos d: vector<pos>{{-1, 0}, {0, 1}, {1, 0}, {0, -1}}) {
        if (visited.find({end, d}) != visited.end() && visited[{end, d}].first == lowest_score) {
            result.insert(visited[{end, d}].second.begin(), visited[{end, d}].second.end());
        }
    }
    return result;
}

int main() {
    string line;
    board<char> b;
    int r = 0;
    while (cin >> line) {
        b.push_back(vector<char>(line.begin(), line.end()));
        r++;
    }
    b.maxr = r;
    b.maxc = line.size();

    auto lowest_score = best_path(b, {b.maxr - 2, 1});
    cout << lowest_score << "\n";
    auto spots = best_spots(b, {b.maxr - 2, 1}, lowest_score);
    cout << spots.size() << "\n";

    //for (auto s: spots) {
    //    b[s] = 'O';
    //}
    //cout << b << "\n";

    return 0;
}
