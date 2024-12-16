#include <iostream>
#include <queue>
#include <set>
#include <optional>
#include <math.h>

#include "util.h"

using namespace std;
using namespace util;

using loc = pair<pos, pos>;

struct PathCost {
    ll prio;
    loc l;

    friend bool operator<(const PathCost& pc1, const PathCost& pc2) {
        return pc1.prio > pc2.prio;
    }

    friend ostream& operator<<(ostream& o, const PathCost& pc) {
        return o << pc.prio << " | " << pc.l.first << ", " << pc.l.second;
    }
};

ll best_path(board<char>& b, pos start, pos end) {
    priority_queue<PathCost> q;
    q.push({0, {start, Dir::E}});
    set<loc> visited;
    visited.insert({start, Dir::E});
    while (!q.empty()) {
        auto [prio, l] = q.top();
        auto [curr, dir] = l;
        q.pop();
        if (curr == end) return prio;

        for (auto n: b.neighbours(curr)) {
            if (visited.find({n, n - curr}) != visited.end()) continue;
            if (b[n] != '#') {
                pos new_dir = n - curr;
                if (new_dir == dir) q.push({prio + 1, {n, new_dir}});
                else q.push({prio + 1001, {n, new_dir}});
                visited.insert({n, new_dir});
            }
        }
    }

    return -1;
}

set<pos> best_spots(board<char>& b, pos start, pos end, ll lowest_score) {
    set<pos> result;
    priority_queue<PathCost> q;
    q.push({0, {start, Dir::E}});
    map<loc, pair<ll, set<loc>>> visited;
    visited[{start, Dir::E}] = {0, {}};
    while (!q.empty()) {
        auto [prio, l] = q.top();
        auto [curr, dir] = l;
        q.pop();
        if (curr == end) continue;

        for (auto n: b.neighbours(curr)) {
            if (b[n] == '#') continue;
            ll cost = (n - curr == dir) ? prio + 1 : prio + 1001;
            if (cost > lowest_score) continue;
            if (visited.find({n, n - curr}) != visited.end()) {
                // If we are here, cost cannot be lower by dijkstra's design
                if (visited[{n, n - curr}].first == cost) {
                    // Add additional predecessor in the path with the same cost
                    visited[{n, n - curr}].second.insert({curr, dir});
                }
                continue;
            }
            if (b[n] != '#') {
                if (n - curr == dir) q.push({prio + 1, {n, dir}});
                else q.push({prio + 1001, {n, n - curr}});
                visited[{n, n - curr}] = {cost, {{curr, dir}}};
            }
        }
    }

    for (pos d: cardinals) {
        if (visited.find({end, d}) != visited.end() && visited[{end, d}].first == lowest_score) {
            // Recreate the path from the back
            queue<loc> qq;
            qq.push({end, d});
            while (!qq.empty()) {
                auto [pp, dd] = qq.front();
                qq.pop();
                result.insert(pp);
                for (auto& l: visited[{pp, dd}].second) qq.push(l);
            }
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
    pos start = {b.maxr - 2, 1};
    pos end = {1, b.maxc - 2};

    auto lowest_score = best_path(b, start, end);
    cout << lowest_score << "\n";
    auto spots = best_spots(b, start, end, lowest_score);
    cout << spots.size() << "\n";

    return 0;
}
