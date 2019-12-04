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

Point findClosest(const std::vector<Point>& points, const Point& p) {
    int minDist = distance(points[0], p);
    Point minPoint = points[0];

    for (int i = 1; i < points.size(); ++i) {
        if (distance(points[i], p) < minDist) {
            minDist = distance(points[i], p);
            minPoint = points[i];
        }
    }

    return minPoint;
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
    
    std::map<Point, int> count;
    int maxCount = 0;
    Point maxPoint = {0, 0}; 

    for (int i = 0; i < 500; ++i) {
        for (int j = 0; j < 500; ++j) {
            Point closest = findClosest(points, {i, j});
            count[closest]++;
        }
    }

    for (int i = 0; i < 500; ++i) {
        Point closest = findClosest(points, {i, 0});
        count[closest] = 0;
        closest = findClosest(points, {i, 499});
        count[closest] = 0;
    }

    for (int i = 0; i < 500; ++i) {
        Point closest = findClosest(points, {0, i});
        count[closest] = 0;
        closest = findClosest(points, {499, i});
        count[closest] = 0;
    }

    for (auto &x: count) {
        if (x.second > maxCount) {
            maxCount = x.second;
            maxPoint = x.first;
        }
    }

    std::cout << maxCount << std::endl;
    std::cout << maxPoint.x << ", " << maxPoint.y << std::endl;

    return 0;
}