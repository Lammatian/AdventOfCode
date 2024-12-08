#include <iostream>
#include <map>
#include <cmath>
#include <set>

#include "util.h"

using namespace std;

struct pos {
    int r;
    int c;
    
    friend ostream& operator<<(ostream& o, pos p) {
        return o << "(" << p.r << ", " << p.c << ")";
    }

    pos& operator+=(const pos& rhs) {
        this->r += rhs.r;
        this->c += rhs.c;
        return *this;
    }

    pos& operator-=(const pos& rhs) {
        this->r -= rhs.r;
        this->c -= rhs.c;
        return *this;
    }

    friend bool operator<(pos p1, pos p2) {
        return p1.r < p2.r or (p1.r == p2.r and p1.c < p2.c);
    }

    friend pos operator+(const pos p1, const pos p2) {
        return {p1.r + p2.r, p1.c + p2.c};
    }

    friend pos operator-(const pos p1, const pos p2) {
        return {p1.r - p2.r, p1.c - p2.c};
    }
};

bool in_bounds(pos x, int maxr, int maxc) {
    return x.r >= 0 && x.r < maxr && x.c >= 0 && x.c < maxc;
}

pair<pos, pos> antinodes(pos a1, pos a2) {
    pos dp = {a1.r - a2.r, a1.c - a2.c};
    return {a1 + dp, a2 - dp};
}

set<pos> get_antinodes(pos a1, pos a2, int maxr, int maxc, bool use_resonance) {
    pos dp = {a1.r - a2.r, a1.c - a2.c};
    set<pos> result;
    if (!use_resonance) {
        auto an1 = a1 + dp;
        auto an2 = a2 - dp;
        if (in_bounds(an1, maxr, maxc)) result.insert(an1);
        if (in_bounds(an2, maxr, maxc)) result.insert(an2);
        return result;
    }
    result = {a1, a2};  
    pos start = a1 + dp;
    while (in_bounds(start, maxr, maxc)) {
        result.insert(start);
        start += dp;
    }
    start = a2 - dp;
    while (in_bounds(start, maxr, maxc)) {
        result.insert(start);
        start -= dp;
    }
    return result;
}

set<pos> all_antinodes_for_freq(const vector<pos>& antennas, int maxr, int maxc, bool use_resonance) {
    set<pos> result;
    for (size_t i = 0; i < antennas.size(); ++i) {
        for (size_t j = i + 1; j < antennas.size(); ++j) {
            auto antinodes = get_antinodes(antennas[i], antennas[j], maxr, maxc, use_resonance); 
            result.insert(antinodes.begin(), antinodes.end());
        }
    }
    return result;
}

set<pos> all_antinodes(const map<char, vector<pos>>& antennas, int maxr, int maxc, bool use_resonance) {
    set<pos> result;
    for (const auto& [k, v]: antennas) {
        auto antinodes = all_antinodes_for_freq(v, maxr, maxc, use_resonance);
        result.insert(antinodes.begin(), antinodes.end());
    }
    return result;
}

int main() {
    string line;
    map<char, vector<pos>> antennas;
    int r = 0;
    while (cin >> line) {
        for (int c = 0; c < line.size(); ++c) {
            if (line[c] != '.') antennas[line[c]].push_back({r, c});
        }
        r++;
    }
    int maxr = r;
    int maxc = line.size();

    auto no_resonance_antinodes = all_antinodes(antennas, maxr, maxc, false);
    cout << no_resonance_antinodes.size() << "\n";
    auto resonance_antinodes = all_antinodes(antennas, maxr, maxc, true);
    cout << resonance_antinodes.size() << "\n";

    return 0;
}
