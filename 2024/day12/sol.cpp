#include <iostream>
#include <set>
#include <queue>

#include "util.h"

using namespace std;
using namespace util;

ll fence_price(board<char>& b, pos start, set<pos>& visited) {
    char t = b[start]; 
    queue<pos> q;
    q.push(start);
    visited.insert(start);
    ll area = 0;
    ll total_perimeter = 0;
    while (!q.empty()) {
        pos curr = q.front();
        area++;
        q.pop();
        // Initial perimeter is 4, remove for each neighbour
        ll perimeter = 4;
        for (auto n: b.neighbours(curr)) {
            if (b[n] != t) continue;
            perimeter--;

            if (visited.find(n) == visited.end()) {
                visited.insert(n);
                q.push(n);
            }
        }
        total_perimeter += perimeter;
    }

    return area * total_perimeter;
}

ll fence_prices(board<char>& b) {
    ll result = 0;
    set<pos> visited; 
    for (int r = 0; r < b.maxr; ++r) {
        for (int c = 0; c < b.maxc; ++c) {
            if (visited.find({r, c}) == visited.end()) {
                result += fence_price(b, {r, c}, visited);
            } 
        }
    }
    return result;
}

ll bulk_fence_area(board<char>& b, pos start, set<pos>& visited, board<int>& rs, int region) {
    char t = b[start]; 
    queue<pos> q;
    q.push(start);
    visited.insert(start);
    ll area = 0;
    while (!q.empty()) {
        pos curr = q.front();
        area++;
        rs[curr] = region;
        q.pop();
        for (auto n: b.neighbours(curr)) {
            if (b[n] != t) continue;

            if (visited.find(n) == visited.end()) {
                visited.insert(n);
                q.push(n);
            }
        }
    }

    return area;
}

// Number of corners per region for a 2x2 square identified by top-left element (`p`)
// E.g.
// 1 2
// 2 3
// has 1 corner for region 1, 2 corners for region 2 and 1 corner for region 3
// 1 1
// 1 2
// has 1 corner for both regions 1 and 2
map<int, ll> corners(board<int>& rs, pos p) {
    map<int, ll> result;
    map<int, int> region_counts;
    region_counts[rs[p]]++;
    region_counts[rs[{p.r, p.c + 1}]]++;
    region_counts[rs[{p.r + 1, p.c}]]++;
    region_counts[rs[{p.r + 1, p.c + 1}]]++;
    for (auto& [k, v]: region_counts) {
        if (region_counts[k] == 1 || region_counts[k] == 3) result[k] = 1;
        if (region_counts[k] == 2) {
            if (rs[p] == k && rs[p + pos{1, 1}] == k) result[k] = 2;
            if (rs[p + pos{0, 1}] == k && rs[p + pos{1, 0}] == k) result[k] = 2;
        }
    }
    return result;
}

ll bulk_discount(board<char>& b, board<int>& rs) {
    ll result = 0;
    map<int, ll> region_to_area;
    set<pos> visited; 
    int regions = 0;
    for (int r = 0; r < b.maxr; ++r) {
        for (int c = 0; c < b.maxc; ++c) {
            if (visited.find({r, c}) == visited.end()) {
                rs[{r, c}] = regions;
                region_to_area[regions] = bulk_fence_area(b, {r, c}, visited, rs, regions);
                regions++;
            } 
        }
    }

    map<int, ll> region_to_sides;
    for (int r = 0; r < rs.maxr - 1; ++r) {
        for (int c = 0; c < rs.maxc - 1; ++c) {
            map<int, ll> region_to_corners = corners(rs, {r, c});
            for (auto [k, v]: corners(rs, {r, c})) {
                region_to_sides[k] += v;
            }
        }
    }

    for (auto [k, v]: region_to_area) {
        if (k == 0) continue;  // Region 0 is the added border
        result += v * region_to_sides[k];
    }

    return result;
}

int main() {
    board<char> b;
    string line;
    int r = 0;
    while (cin >> line) {
        b.push_back(vector<char>(line.begin(), line.end()));
        r++;
    }
    b.maxr = r;
    b.maxc = b[0].size();

    cout << fence_prices(b) << "\n";

    // Idea: Add a border and then look at all 2x2 squares and count corners
    // Number of corners == number of sides
    vector<char> border_row(b.maxc + 2, '.');
    b.insert(b.begin(), border_row);
    for (size_t r = 1; r <= b.maxr; ++r) {
        b[r].push_back('.');
        b[r].insert(b[r].begin(), '.');
    }
    b.push_back(border_row);
    b.maxr += 2;
    b.maxc += 2;
    board<int> regions(b.maxr, b.maxc);

    cout << bulk_discount(b, regions) << "\n";

    return 0;
}
