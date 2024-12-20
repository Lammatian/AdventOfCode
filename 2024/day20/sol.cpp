#include <iostream>
#include <map>
#include <queue>

#include "util.h"

using namespace std;
using namespace util;

vector<pos> recreate_path(map<pos, pos>& visited, pos end) {
    vector<pos> result = {end};
    pos curr = end;
    while (visited[curr] != curr) {
        curr = visited[curr];
        result.push_back(curr);
    }

    reverse(result.begin(), result.end());
    return result;
}

pair<ll, vector<pos>> fastest_path(const board<char>& b, pos start, pos end) {
    queue<pair<pos, ll>> q;
    q.push({start, 0});
    map<pos, pos> visited;
    visited[start] = start;
    while (!q.empty()) {
        auto [curr, d] = q.front();
        q.pop();
        if (curr == end) return {d, recreate_path(visited, end)};
        for (auto n: b.neighbours(curr)) {
            if (b[n] == '#') continue;
            if (visited.find(n) != visited.end()) continue;
            q.push({n, d + 1});
            visited[n] = curr;
        }
    }

    return {-1, {}};
}

board<ll> distances_to_end(const board<char>& b, pos start, pos end) {
    board<ll> ds(b.maxr, b.maxc, -1);
    queue<pair<pos, ll>> q;
    q.push({end, 0});
    ds[end] = 0;
    set<pos> visited;
    visited.insert(end);
    while (!q.empty()) {
        auto [curr, d] = q.front();
        q.pop();
        for (auto n: b.neighbours(curr)) {
            if (b[n] == '#') continue;
            if (visited.find(n) != visited.end()) continue;
            q.push({n, d + 1});
            ds[n] = d + 1;
            visited.insert(n);
        }
    }
    return ds;
}

vector<ll> possible_savings(const board<char>& b, const board<ll>& d, pos p, int max_dist) {
    set<pos> visited;
    vector<ll> result;
    queue<pair<pos, int>> q;
    q.push({p, 0});
    while (!q.empty()) {
        auto [curr, dist] = q.front();
        q.pop();
        if (d[curr] >= 0 && d[curr] + dist < d[p]) {
            result.push_back(d[p] - d[curr] - dist);
        }
        for (auto n: b.neighbours(curr)) {
            if (visited.find(n) != visited.end()) continue;
            if (dist == max_dist) continue;
            visited.insert(n);
            q.push({n, dist + 1});
        }
    }

    return result;
}

int main(int argc, char** argv) {
    ll min_savings = stoll(argv[1]);
    string line;
    board<char> b;
    pos start, end;
    while (cin >> line) {
        b.push_back(vector<char>(line.begin(), line.end()));
        for (int c = 0; c < line.size(); ++c) {
            if (line[c] == 'S') start = {(int)b.size() - 1, c};
            if (line[c] == 'E') end = {(int)b.size() - 1, c};
        }
    }
    b.maxr = b.size();
    b.maxc = b[0].size();

    auto [time, path] = fastest_path(b, start, end);
    board<ll> d = distances_to_end(b, start, end);
    ll result1 = 0;
    ll result2 = 0;
    for (auto p: path) {
        auto savings1 = possible_savings(b, d, p, 2);
        auto savings2 = possible_savings(b, d, p, 20);
        for (auto s: savings1) {
            if (s >= min_savings) result1++;
        }
        for (auto s: savings2) {
            if (s >= min_savings) result2++;
        }
    }
    cout << result1 << "\n";
    cout << result2 << "\n";

    return 0;
}
