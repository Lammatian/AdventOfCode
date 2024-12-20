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

ll dist(pos p1, pos p2) {
    return abs(p1.r - p2.r) + abs(p1.c - p2.c);
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

    ll result1 = 0;
    ll result2 = 0;
    auto [time, path] = fastest_path(b, start, end);
    // I only need to check points on the path since the input is 'special'
    // and all empty spots are part of the path
    for (int i = 0; i < path.size(); ++i) {
        for (int j = i + 1; j < path.size(); ++j) {
            ll ds = dist(path[j], path[i]);
            if (ds <= 2 && j - i - ds >= min_savings) result1++;
            if (ds <= 20 && j - i - ds >= min_savings) result2++;
        }
    }
    cout << result1 << "\n";
    cout << result2 << "\n";

    return 0;
}
