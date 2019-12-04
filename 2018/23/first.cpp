#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

typedef long long ll;

struct Position {
    ll x;
    ll y;
    ll z; 
    ll r;
};

ll distance(const Position& a, const Position& b) {
    return std::abs(a.x - b.x) + std::abs(a.y - b.y) + std::abs(a.z - b.z);
}

int main() {
    std::fstream f("input.txt");
    std::vector<Position> pos;
    Position best{0, 0, 0, 0};
    std::string token;

    while (getline(f, token, '<')) {
        getline(f, token, ',');
        ll x = std::stoll(token);
        getline(f, token, ',');
        ll y = std::stoll(token);
        getline(f, token, '>');
        ll z = std::stoll(token);
        getline(f, token, '=');
        getline(f, token);
        ll r = std::stoll(token);
        pos.push_back({x, y, z, r});

        if (r > best.r) {
            best = {x, y, z, r};
        }
    }

    int result = 0;

    for (auto& p: pos) {
        if (distance(best, p) <= best.r) {
            result++;
        }
    }

    std::cout << result << std::endl;

    return 0;
}