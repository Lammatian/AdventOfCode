#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <cmath>

using namespace std;

struct pos {
    int r;
    int c;
};

int operator<(pos a, pos b) {
    return a.r < b.r or (a.r == b.r and a.c < b.c);
}

bool operator==(pos a, pos b) {
    return a.r == b.r and a.c == b.c;
}

pos operator+(pos a, pos b) {
    return {a.r + b.r, a.c + b.c};
}

ostream& operator<<(ostream& o, pos p) {
    return o << "(" << p.r << ", " << p.c << ")";
}

template<typename T>
ostream& operator<<(ostream& o, const vector<T>& v) {
    o << "[";
    for (const auto& x: v) {
        o << x << " ";
    }
    return o << "]";
}

struct board {
    set<pos> obstacles;
    map<int, vector<int>> obs_by_row;
    map<int, vector<int>> obs_by_col;
    int maxr;   
    int maxc;
};

bool in_bounds(pos p, const board& b) {
    return p.r >= 0 and p.c >= 0 and p.r < b.maxr and p.c < b.maxc;
}

pos rotate(pos x) {
    if (x.r == -1) return {0, 1};
    else if (x.c == 1) return {1, 0};
    else if (x.r == 1) return {0, -1};
    else if (x.c == -1) return {-1, 0};
    else throw "unexpected direction to rotate";
}

pos next(board& b, pos curr, pos dir) {
    if (dir.r == -1) {
        // find same column, smaller row
        if (b.obs_by_col[curr.c].empty() or b.obs_by_col[curr.c][0] >= curr.r) return {-1, curr.c};
        else {
            for (auto it = b.obs_by_col[curr.c].rbegin(); it != b.obs_by_col[curr.c].rend(); ++it) {
                if (*it < curr.r) {
                    return {*it + 1, curr.c};
                }
            }
        }
    }
    if (dir.c == 1) {
        // find same row, larger column
        if (b.obs_by_row[curr.r].empty() or b.obs_by_row[curr.r].back() <= curr.c) return {curr.r, b.maxc};
        else {
            for (auto c: b.obs_by_row[curr.r]) {
                if (c > curr.c) {
                    return {curr.r, c - 1};
                }
            }
        }
    }
    if (dir.r == 1) {
        // find same column, larger row
        if (b.obs_by_col[curr.c].empty() or b.obs_by_col[curr.c].back() <= curr.r) return {b.maxr, curr.c};
        else {
            for (auto r: b.obs_by_col[curr.c]) {
                if (r > curr.r) {
                    return {r - 1, curr.c};
                }
            }
        }
    }
    if (dir.c == -1) {
        // find same row, smaller column
        if (b.obs_by_row[curr.r].empty() or b.obs_by_row[curr.r][0] >= curr.c) return {curr.r, -1};
        else {
            for (auto it = b.obs_by_row[curr.r].rbegin(); it != b.obs_by_row[curr.r].rend(); ++it) {
                if (*it < curr.c) {
                    return {curr.r, *it + 1};
                }
            }
        }
    }
    return {-1, -1};
}

int dist(pos a, pos b) {
    return std::abs(a.r - b.r) + std::abs(a.c - b.c);
}

// TODO: Not sure how to smartly use this for part 1
bool walk2(board& b, pos curr, pos dir, set<pair<pos, pos>>& visited) {
    visited.insert({curr, dir});
    pos newpos = next(b, curr, dir);
    if (visited.find({newpos, rotate(dir)}) != visited.end()) {
        return true;
    }
    if (!in_bounds(newpos, b)) {
        return false;
        //return dist(curr, newpos) - 1;
    } else {
        return walk2(b, newpos, rotate(dir), visited);
        //return dist(curr, newpos) + walk2(b, newpos, rotate(dir), visited);
    }
}

// Returns true if looped
bool walk(const board& b, pos curr, pos dir, set<pair<pos, pos>>& visited) {
    visited.insert({curr, dir});

    pos newpos = curr + dir; 
    if (visited.find({newpos, dir}) != visited.end()) {
        return true;
    } else if (b.obstacles.find(newpos) != b.obstacles.end()) {
        return walk(b, curr, rotate(dir), visited);
    } else if (!in_bounds(newpos, b)) {
        return false;
    }

    return walk(b, newpos, dir, visited);
}

int main() {
    string line;
    board b; 
    int r = 0;
    pos start;
    while (cin >> line) {
        for (int c = 0; c < line.size(); ++c) {
            if (line[c] == '#') {
                b.obstacles.insert({r, c});
                b.obs_by_row[r].push_back(c);
                b.obs_by_col[c].push_back(r);
            } else if (line[c] == '^') {
                start = {r, c};
            }
        }
        r++;
    }
    for (auto& [k, v]: b.obs_by_row) {
        sort(v.begin(), v.end());
    }
    for (auto& [k, v]: b.obs_by_col) {
        sort(v.begin(), v.end());
    }

    b.maxr = r;
    b.maxc = line.size(); 

    set<pair<pos, pos>> visited;
    walk(b, start, {-1, 0}, visited);
    set<pos> visited_locs;
    for (auto [p, d]: visited) {
        visited_locs.insert(p);
    }
    cout << visited_locs.size() << "\n";
    
    // Part 2
    // Idea #1: Check ALL the positions for a new obstacle - very slow but got me a star [79.1s]
    // int result = 0;
    // for (int r = 0; r < b.maxr; ++r) {
    //     for (int c = 0; c < b.maxc; ++c) {
    //         if (b.obstacles.find({r, c}) != b.obstacles.end()) continue;
    //         b.obstacles.insert({r, c});
    //         visited = set<pair<pos, pos>>();
    //         if (walk(b, start, {-1, 0}, visited)) {
    //             result++;
    //         }
    //         b.obstacles.erase({r, c});
    //     }
    // }
    // cout << result << "\n";

    // Idea #2: Check only the positions that were visited in the first place [16.5s]
    // int result = 0;
    // for (auto p: visited_locs) {
    //     b.obstacles.insert(p);
    //     auto visited2 = set<pair<pos, pos>>();
    //     if (walk(b, start, {-1, 0}, visited2)) {
    //         result++;
    //     }
    //     b.obstacles.erase(p);
    // }
    // cout << result << "\n";

    // Idea #3: Only consider turn locations when walking, avoid simulating all the intermediate steps [0.7s]
    int result = 0;
    for (auto p: visited_locs) {
        auto& obr = b.obs_by_row[p.r];
        auto& obc = b.obs_by_col[p.c];
        obr.push_back(p.c);
        obc.push_back(p.r);
        sort(obr.begin(), obr.end());
        sort(obc.begin(), obc.end());
        auto visited2 = set<pair<pos, pos>>();
        if (walk2(b, start, {-1, 0}, visited2)) {
            result++;
        }
        for (auto it = obr.begin(); it != obr.end(); ++it) {
            if (*it == p.c) {
                obr.erase(it);
                break;
            }
        }
        for (auto it = obc.begin(); it != obc.end(); ++it) {
            if (*it == p.r) {
                obc.erase(it);
                break;
            }
        }
    }
    cout << result << "\n";

    // Idea #4: For each three turns, add a 4th one and see if on the path. If yes, will create a loop - I'm too lazy to implement this

    return 0;
}
