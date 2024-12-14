#include <iostream>
#include <vector>
#include <map>
#include <unistd.h>

#include "util.h"

using namespace std;
using namespace util;

struct robot {
    int x;
    int y;
    int dx;
    int dy;

    robot(const string& s1, const string& s2) {
        auto p = split(split(s1, "=")[1], ",");
        x = stoi(p[0]);
        y = stoi(p[1]);
        auto v = split(split(s2, "=")[1], ",");
        dx = stoi(v[0]);
        dy = stoi(v[1]);
    }

    friend ostream& operator<<(ostream& o, robot r) {
        return o << "r(" << r.x << ", " << r.y << ", " << r.dx << ", " << r.dy << ")";
    }

    void move(int seconds, int X, int Y) {
        x = ((x + seconds * dx) % X + X) % X;
        y = ((y + seconds * dy) % Y + Y) % Y;
    }

    int quadrant(int X, int Y) {
        if (x < X / 2 && y < Y / 2) return 1;
        if (x < X / 2 && y > Y / 2) return 2;
        if (x > X / 2 && y < Y / 2) return 3;
        if (x > X / 2 && y > Y / 2) return 4;
        return -1;
    }
};

pair<float, float> mean(const vector<robot>& rs) {
    float mx=0, my=0;
    for (const auto& r: rs) {
        mx += r.x;
        my += r.y;
    }
    return {mx / rs.size(), my / rs.size()};
}

pair<float, float> variance(const vector<robot>& rs) {
    float vx=0, vy=0;
    auto [mx, my] = mean(rs);
    for (const auto& r: rs) {
        vx += (r.x - mx) * (r.x - mx);
        vy += (r.y - my) * (r.y - my);
    }
    return {vx / rs.size(), vy / rs.size()};
}

void show_robots(const vector<robot>& rs, int X, int Y) {
    vector<string> board(Y, string(X, '.'));
    for (const auto& r: rs) {
        board[r.y][r.x] = '#';
    }

    cout << "\n";
    for (auto& row: board) {
        cout << row << "\n";
    }
    cout << "\n";
}

int main(int argc, char** argv) {
    int X = stoi(argv[1]);
    int Y = stoi(argv[2]);
    string s1, s2;
    map<int, ll> quadrant_counts;
    vector<robot> robots;
    while (cin >> s1) {
        cin >> s2;
        robot r(s1, s2);
        r.move(100, X, Y);
        robots.push_back(r);
        quadrant_counts[r.quadrant(X, Y)]++;
    }

    ll result = 1;
    for (int i = 1; i <= 4; ++i) {
        result *= quadrant_counts[i];
    }
    cout << result << "\n";
    auto [start_vx, start_vy] = variance(robots);
    float vx = start_vx;
    float vy = start_vy;

    int steps = 100;
    // This even 'works' on sample. Variance of 0.25 seems to run forever on sample
    while (vx * vy > start_vx * start_vy * 0.35) {
        steps++;
        for (auto& r: robots) {
            r.move(1, X, Y);
        }
        auto v = variance(robots);
        vx = v.first;
        vy = v.second;
    }
    cout << steps << "\n";
    // show_robots(robots, X, Y);

    // Experiments
    // show_robots(robots, X, Y);
    // for (int i = 0; i < 10'000; ++i) {
    //     ll robots_in_middle = 0;
    //     for (auto& r: robots) {
    //         r.move(1, X, Y);
    //         if (r.x == X / 2) robots_in_middle++;
    //     }
    //     if (robots_in_middle > 10) {
    //         cout << i + 100 << "\n";
    //         show_robots(robots, X, Y);
    //         usleep(50000);
    //     }
    // }

    return 0;
}
