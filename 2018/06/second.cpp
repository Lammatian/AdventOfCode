#include <iostream>
#include <vector>
#include <fstream>
#include <cmath>
#include <string>
#include <set>
#include <stack>
#include <map>
#include <algorithm>

struct Point {
    int x, y;

    bool operator==(const Point& a) const {
        return a.x == x && a.y == y;
    }

    bool operator!=(const Point& a) const {
        return !(*this == a);
    }

    bool operator<(const Point& a) const {
        return x < a.x || (x == a.x && y < a.y);
    }
};

int distance(const Point& p1, const Point& p2) {
    return std::abs(p1.x - p2.x) + std::abs(p1.y - p2.y);
}

int distanceToAll(const std::vector<Point>& points, const Point& p) {
    int result = 0;

    for (int i = 0; i < points.size(); ++i) {
        result += distance(points[i], p);
    }

    return result;
}

int main() {
    std::fstream f("input.txt");
    std::vector<Point> points;
    std::string token;

    while (getline(f, token, ',')) {
        int x = std::stoi(token);
        getline(f, token);
        int y = std::stoi(token);
        points.push_back({x, y});
    }
    
    int count = 0;

    for (int i = 0; i < 500; ++i) {
        for (int j = 0; j < 500; ++j) {
            if (distanceToAll(points, {i, j}) < 10000) {
                count++;
            }
        }
    }

    for (int i = 0; i < 500; ++i) {
        if (distanceToAll(points, {i, 0}) < 10000 || distanceToAll(points, {i, 499}) < 10000) {
            std::cout << i << ",0/499" << std::endl;
        }
    }

    for (int i = 0; i < 500; ++i) {
        if (distanceToAll(points, {0, i}) < 10000 || distanceToAll(points, {499, i}) < 10000) {
            std::cout << "0/499," << i << std::endl;
        }
    }

    std::cout << count << std::endl;

    return 0;
}
