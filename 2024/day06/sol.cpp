#include <iostream>
#include <vector>
#include <set>
#include <map>

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

struct board {
    set<pos> obstacles;
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
            } else if (line[c] == '^') {
                start = {r, c};
            }
        }
        r++;
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
    int result = 0;
    for (auto p: visited_locs) {
        b.obstacles.insert(p);
        auto visited2 = set<pair<pos, pos>>();
        if (walk(b, start, {-1, 0}, visited2)) {
            result++;
        }
        b.obstacles.erase(p);
    }
    cout << result << "\n";

    // Idea #3: For each three turns, add a 4th one and see if on the path. If yes, will create a loop - I'm too lazy to implement this
    // Idea #4: Instead of iterating each move, just 'jump' to the next turn. Again, too lazy to implement

    return 0;
}
