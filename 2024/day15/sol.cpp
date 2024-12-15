#include <iostream>
#include <map>
#include <set>
#include <ranges>
#include <numeric>
#include <queue>
#include <unistd.h>

#include "util.h"

using namespace std;
using namespace util;

bool move(board<char>& b, pos d, pos& start) {
    vector<pos> pos_to_move = {start};
    pos curr = start + d;
    while (b.in_bounds(curr) && b[curr] != '#' && b[curr] != '.') {
        pos_to_move.push_back(curr);
        curr += d;
    }

    if (!b.in_bounds(curr) || b[curr] == '#') return false;

    for (auto p: pos_to_move | views::reverse) {
        b[p + d] = b[p];
        b[p] = '.';
    }

    start += d;
    return true;
}

void simulate(board<char>& b, const vector<pos>& moves, pos start) {
    for (auto m: moves) {
        move(b, m, start);
    }
}

bool move2(board<char>& b, pos d, pos& start) {
    vector<pos> pos_to_move;
    queue<pos> q;
    q.push(start);
    set<pos> visited;
    while (!q.empty()) {
        pos curr = q.front();
        q.pop();
        if (visited.find(curr) != visited.end()) continue;
        pos_to_move.push_back(curr);
        visited.insert(curr);
        pos next = curr + d;
        if (b[next] == '#') return false;
        if (b[next] == '[') {
            q.push(next);
            if (d.r != 0) q.push(next + pos{0, 1});
        }
        if (b[next] == ']') {
            q.push(next);
            if (d.r != 0) q.push(next + pos{0, -1});
        }
    }

    for (auto p: pos_to_move | views::reverse) {
        b[p + d] = b[p];
        b[p] = '.';
    }

    start += d;
    return true;
}

void simulate2(board<char>& b, const vector<pos>& moves, pos start) {
    for (auto m: moves) {
        move2(b, m, start);
    }
}

int main() {
    string line;
    board<char> b;
    board<char> b2;
    bool reading_board = true;
    vector<pos> moves;
    int r = 0;
    int R, C;
    pos start;
    pos start2;
    while (getline(cin, line)) {
        if (line == "") reading_board = false;
        if (reading_board) {
            b.push_back(vector<char>(line.begin(), line.end()));
            b2.push_back(vector<char>());
            for (int c = 0; c < line.size(); ++c) {
                if (line[c] == '@') {
                    start = {r, c};
                    start2 = {r, 2*c};
                }
                switch (line[c]) {
                    case '#':
                        b2[r].push_back('#');
                        b2[r].push_back('#');
                        break;
                    case 'O':
                        b2[r].push_back('[');
                        b2[r].push_back(']');
                        break;
                    case '.':
                        b2[r].push_back('.');
                        b2[r].push_back('.');
                        break;
                    case '@':
                        b2[r].push_back('@');
                        b2[r].push_back('.');
                        break;
                }
            }
            r++;
            C = line.size();
        } else {
            for (auto c: line) {
                switch (c) {
                    case '^':
                        moves.push_back({-1, 0});
                        break;
                    case '>':
                        moves.push_back({0, 1});
                        break;
                    case 'v':
                        moves.push_back({1, 0});
                        break;
                    case '<':
                        moves.push_back({0, -1});
                        break;
                }
            }   
        }
    }
    R = r;

    b.maxr = R;
    b.maxc = C;
    simulate(b, moves, start);
    ll result = 0;
    for (int r = 0; r < b.maxr; ++r) {
        for (int c = 0; c < b.maxc; ++c) {
            if (b[r][c] == 'O') result += 100*r + c;
        }
    }
    cout << result << "\n";

    b2.maxr = R;
    b2.maxc = 2 * C;
    simulate2(b2, moves, start2);
    ll result2 = 0;
    for (int r = 0; r < b2.maxr; ++r) {
        for (int c = 0; c < b2.maxc; ++c) {
            if (b2[r][c] == '[') result2 += 100*r + c;
        }
    }
    cout << result2 << "\n";

    return 0;
}
