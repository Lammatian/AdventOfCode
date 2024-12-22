#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <print>

#include "util.h"

using namespace std;
using namespace util;

// Find target character on the board
pos find(const board<char>& b, char target) {
    for (int r = 0; r < b.maxr; ++r) {
        for (int c = 0; c < b.maxc; ++c) {
            if (b[r][c] == target) {
                return {r, c};
            }
        }
    }

    throw "Target not found";
}

// Check if a string is sorted in a very disgusting way
bool sorted(const string& s) {
    string tmp1{s};
    string tmp2{s};
    sort(tmp1.begin(), tmp1.end());
    sort(tmp2.begin(), tmp2.end());
    reverse(tmp2.begin(), tmp2.end());
    return s == tmp1 || s == tmp2;
}

/**
 * Return all (sorted) shortest paths from `s` to `e`
 */
vector<string> shortest_paths(const board<char>& b, char s, char e) {
    map<pos, string> dir_to_path = {
        {{-1, 0}, "^"},
        {{0, 1}, ">"},
        {{1, 0}, "v"},
        {{0, -1}, "<"}
    };
    pos start = find(b, s);
    pos end = find(b, e);
    queue<pair<pos, string>> q;
    q.push({start, ""});
    vector<string> result;
    while (!q.empty()) {
        auto [curr, path] = q.front(); q.pop();
        /**
         * This is an optimisation - seems like we can ignore paths that have
         * alternating directions
         * e.g. <<^^ will always be cheaper than <^<^
         * which intuitively makes sense
         */
        if (path.size() > 1 && !sorted(path)) continue;
        if (!result.empty() && path.size() == result[0].size()) continue;
        if (curr == end) {
            if (result.empty()) result.push_back(path + "A");
            else if (path.size() == result[0].size() - 1) result.push_back(path + "A");
        }
        for (auto n: b.neighbours(curr)) {
            if (b[n] == '.') continue;
            q.push({n, path + dir_to_path[n - curr]});
        }
    }

    return result;
}

/**
 * Get all possible shortest paths between all characters on both button pads
 * E.g. for num pad, get the shortest paths between 'A' and '2' (<^ and ^<)
 *      for robo pad, get the shortest paths between '<' and '>' (only >>)
 */
map<pair<char, char>, vector<string>> compute_paths() {
    map<pair<char, char>, vector<string>> result;
    board<char> numpad(4, 3);
    numpad[0] = {'7', '8', '9'};
    numpad[1] = {'4', '5', '6'};
    numpad[2] = {'1', '2', '3'};
    numpad[3] = {'.', '0', 'A'};
    string numpad_buttons = "A0123456789";
    for (int i = 0; i < numpad_buttons.size(); ++i) {
        for (int j = 0; j < numpad_buttons.size(); ++j) {
            char b1 = numpad_buttons[i];
            char b2 = numpad_buttons[j];
            if (i == j) result[{b1, b2}] = {"A"};
            else result[{b1, b2}] = shortest_paths(numpad, b1, b2);
        }
    }
    board<char> robopad(2, 3);
    robopad[0] = {'.', '^', 'A'};
    robopad[1] = {'<', 'v', '>'};
    string robopad_buttons = "^<v>A";
    for (int i = 0; i < robopad_buttons.size(); ++i) {
        for (int j = 0; j < robopad_buttons.size(); ++j) {
            char b1 = robopad_buttons[i];
            char b2 = robopad_buttons[j];
            if (i == j) result[{b1, b2}] = {"A"};
            else result[{b1, b2}] = shortest_paths(robopad, b1, b2);
        }
    }

    return result;
}

struct level_path {
    pair<char, char> from_to;
    ll level;

    friend bool operator<(const level_path& k1, const level_path& k2) {
        return k1.from_to < k2.from_to ||
            k1.from_to == k2.from_to && k1.level < k2.level;
    }

    friend ostream& operator<<(ostream& o, const level_path& k) {
        return o << "(" << k.from_to << ", " << k.level << ")";
    }
};

ll shortest_seq_len(map<level_path, ll>& bottom_up, const string& seq, ll level) {
    ll result = 0;
    for (int i = 0; i < seq.size(); ++i) {
        if (i == 0) result += bottom_up[{{'A', seq[i]}, level}];
        else result += bottom_up[{{seq[i - 1], seq[i]}, level}];
    }
    return result;
}

/**
 * Return lengths of shortest paths between buttons at different levels of indirection
 * E.g. the shortest path between 'A' and '3' at indirection level 1 would be
 *      <A>A (length 4), because the direct path (i.e. path at indirection level 0) is ^A
 * Some other examples
 * - Shortest path between '3' and '7' at ID 0 is 4 (e.g. <<^^ or ^^<<)
 * - Shortest path between '3' and '7' at ID 2 is 23 with sequence as follows:
 *     At ID 0 we have '<<^^A'
 *     At ID 1 we have 'v<<AA>^AA>A'
 *     At ID 2 we have 'v<A<AA>>^AAvA<^A>AAvA^A'
 *
 * Params
 *   paths: direct paths between different buttons
 *   levels: number of indirection levels to include in the result
 */
map<level_path, ll> shortest_indirect_paths(
        map<pair<char, char>, vector<string>>& paths,
        int levels) {
    map<level_path, ll> result;
    for (auto& [k, v]: paths) {
        result[{k, 0}] = v[0].size();
    }

    for (int i = 1; i <= levels; ++i) {
        for (auto& [k, v]: paths) {
            result[{k, i}] = accumulate(
                    v.begin() + 1,
                    v.end(),
                    shortest_seq_len(result, v.front(), i - 1),
                    [&result, i](ll acc, string s) {
                        return min(acc, shortest_seq_len(result, s, i - 1));
                    });
        }
    }

    return result;
}

int main() {
    /**
     * Idea: quite classic DP
     * An 'indirect path' between two buttons is a path that has to be taken
     * through robots. So if a direct path between 'A' and '2' on a numpad would
     * be either '<^A' or '^<A' (including the press at the end), then there are
     * multiple different possible paths with '1 robot of indirection':
     *   'v<<A>^A>A' to first press <, then ^, then A
     *   '<v<A>^A>A' to first press <, then ^, then A
     *   '<Av<A>>^A' to first press ^, then >, then A
     *   '<Av<A>^>A' to first press ^, then >, then A
     * Now, I make a few assumptions:
     * 1. A path that is 'turning a lot' is worse than a one-turn path
     *    E.g. I will always prefer >>^^ over >^>^ to get a shortest result
     *    This is not necessary and solution without this is still fast
     * 2. A path that is longer will always result in a longer path with more
     *    indirection. E.g. I don't want to get from 'A' to '2' with '^^<vA' ever
     *    I haven't tested what happens if I include such paths, too much work
     *
     * Now, the idea is to construct a matrix that for each starting button, each
     * ending button and each level of indirection (up to 25) contains the shortest
     * path between the two buttons (including the press) at that level of indirection
     * Then the result is just sum of the paths between consecutive characters
     * (including the path from 'A' to the first character)
     */
    auto direct_paths = compute_paths();
    auto indirect_paths = shortest_indirect_paths(direct_paths, 25);

    string line;
    ll result1 = 0;
    ll result2 = 0;
    while (cin >> line) {
        result1 += stoll(line.substr(0, 3)) * shortest_seq_len(indirect_paths, line, 2);
        result2 += stoll(line.substr(0, 3)) * shortest_seq_len(indirect_paths, line, 25);
    }
    cout << result1 << "\n";
    cout << result2 << "\n";

    return 0;
}
