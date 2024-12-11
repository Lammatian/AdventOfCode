#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <numeric>

#include "util.h"

using namespace std;
using namespace util;

vector<pos> neighbours(const vector<vector<int>>& b, pos p, int maxr, int maxc) {
    vector<pos> ds = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    vector<pos> result;
    for (auto d: ds) {
        if (in_bounds(p + d, maxr, maxc)) {
            result.push_back(p + d);
        }
    }
    return result;
}

ll bfs(const vector<vector<int>>& b, pos start, bool avoid_reps) {
    ll result = 0;
    int maxr = b.size();
    int maxc = b[0].size();
    queue<pos> q;
    q.push(start);
    set<pos> visited;
    visited.insert(start);
    while (!q.empty()) {
        auto curr = q.front();
        q.pop();
        if (b[curr.r][curr.c] == 9) {
            result++;
            continue;
        }
        for (auto n: neighbours(b, curr, maxr, maxc)) {
            if (visited.find(n) == visited.end() && b[n.r][n.c] == b[curr.r][curr.c] + 1) {
                q.push(n);
                if (avoid_reps) visited.insert(n);
            }
        }
    }
    return result;
}

int main() {
    vector<vector<int>> board;
    string line;
    int r = 0;
    vector<pos> starts;
    while (cin >> line) {
        board.push_back(vector<int>());
        for (int c = 0; c < line.size(); ++c) {
            board[r].push_back(line[c] - '0');
            if (line[c] == '0') starts.push_back({r, c});
        }
        r++;
    }

    cout << accumulate(starts.begin(), starts.end(), 0, [&board](ll acc, pos x) { return acc + bfs(board, x, true); }) << "\n";
    cout << accumulate(starts.begin(), starts.end(), 0, [&board](ll acc, pos x) { return acc + bfs(board, x, false); }) << "\n";

    return 0;
}
