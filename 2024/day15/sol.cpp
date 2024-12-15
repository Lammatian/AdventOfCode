#include <iostream>
#include <map>
#include <ranges>
#include <numeric>

#include "util.h"

using namespace std;
using namespace util;

void show_board(map<pos, char>& board, pos robot, int R, int C) {
    vector<string> outboard(R, string(C, '.'));
    for (auto& [k, v]: board) {
        outboard[k.r][k.c] = v;
    }
    outboard[robot.r][robot.c] = '@';

    for (auto& r: outboard) {
        cout << r << "\n";
    }
    cout << "\n";
}

bool point_in_bounds(pos p, int R, int C) {
    return p.r >= 0 && p.r < R && p.c >= 0 && p.c < C;
}

bool move(map<pos, char>& board, pos move, pos& start, int R, int C) {
    vector<pos> stuff_to_move{start};
    pos curr = start + move;
    while (point_in_bounds(curr, R, C) && board.find(curr) != board.end() && board[curr] != '#') {
        stuff_to_move.push_back(curr);
        curr += move;
    }

    if (!point_in_bounds(curr, R, C) || board[curr] == '#') {
        return false;
    }

    for (auto p: stuff_to_move | views::reverse) {
        board[p + move] = board[p];
        board.erase(p); 
    }
    start += move;

    return true;
}

void simulate(map<pos, char>& board, const vector<pos>& moves, pos start, int R, int C) {
    for (auto m: moves) {
        move(board, m, start, R, C);
    } 
}

int main() {
    string line;
    bool reading_board = true;
    map<pos, char> board;
    vector<pos> moves;
    int r = 0;
    int R, C;
    pos robot;
    while (getline(cin, line)) {
        if (line == "") reading_board = false;
        if (reading_board) {
            for (int c = 0; c < line.size(); ++c) {
                if (line[c] == '@') robot = {r, c};
                if (line[c] != '.') board[{r, c}] = line[c]; 
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

    simulate(board, moves, robot, R, C);
    cout << accumulate(board.begin(), board.end(), 0LL, [](ll acc, auto& b) {
            if (b.second == 'O') return acc + 100LL * b.first.r + b.first.c;
            return acc;
            }) << "\n";

    return 0;
}
