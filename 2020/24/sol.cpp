#include <iostream>
#include <fstream>
#include <set>

#include "util.h"

using namespace std;

// https://www.redblobgames.com/grids/hexagons/
// Cube coordinates
struct HexDir {
    int x, y, z;
};

bool operator<(const HexDir& h1, const HexDir& h2) {
    return h1.x < h2.x
        || (h1.x == h2.x && h1.y < h2.y)
        || (h1.x == h2.x && h1.y == h2.y && h1.z < h2.z);
}

HexDir get_hex(const string& directions) {
    HexDir result {0, 0, 0};
    char last = ' ';

    for (auto& c: directions) {
        if (c == 'e') {
            if (last == ' ') {
                result.x++;
                result.y--;
            } else if (last == 's') {
                result.z++;
                result.y--;
            } else {
                result.x++;
                result.z--;
            }

            last = ' ';
        } else if (c == 'w') {
            if (last == ' ') {
                result.x--;
                result.y++;
            } else if (last == 's') {
                result.x--;
                result.z++;
            } else {
                result.z--;
                result.y++;
            }
            
            last = ' ';
        } else if (c == 's') {
            last = 's';
        } else {
            last = 'n';
        }
    }

    return result;
}

ll sol1(vector<string>& lines) {
    set<HexDir> flipped;

    for (auto& line: lines) {
        HexDir cur = get_hex(line);

        if (flipped.find(cur) == flipped.end()) {
            flipped.emplace(cur);
        } else {
            flipped.erase(cur);
        }
    }

    return flipped.size();
}

vector<HexDir> neighbours(HexDir h) {
    vector<HexDir> result;
    result.push_back({h.x + 1, h.y, h.z - 1});
    result.push_back({h.x + 1, h.y - 1, h.z});
    result.push_back({h.x, h.y + 1, h.z - 1});
    result.push_back({h.x, h.y - 1, h.z + 1});
    result.push_back({h.x - 1, h.y, h.z + 1});
    result.push_back({h.x - 1, h.y + 1, h.z});
    return result;
}

set<HexDir> evolve(set<HexDir>& state) {
    set<HexDir> evolution;
    map<HexDir, int> ncount;

    for (auto& h: state) {
        for (auto& nh: neighbours(h)) {
            ncount[nh]++;
        }
    }

    for (auto& entry: ncount) {
        HexDir h = entry.first;
        int count = entry.second;

        if (state.find(h) != state.end() && (count == 1 || count == 2)) {
            evolution.emplace(h);
        } else if (state.find(h) == state.end() && count == 2) {
            evolution.emplace(h);
        }
    }

    return evolution;
}

ll sol2(vector<string>& lines) {
    set<HexDir> flipped;

    for (auto& line: lines) {
        HexDir cur = get_hex(line);

        if (flipped.find(cur) == flipped.end()) {
            flipped.emplace(cur);
        } else {
            flipped.erase(cur);
        }
    }

    for (int i = 0; i < 100; ++i) {
        flipped = evolve(flipped);
    }

    return flipped.size();
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);
    auto lines = util::readlines(f);

    cout << sol1(lines) << endl;
    cout << sol2(lines) << endl;

    return 0;
}
