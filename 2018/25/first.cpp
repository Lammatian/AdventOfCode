#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <string>
#include <cmath>
#include <queue>

struct Point {
    int x;
    int y;
    int z;
    int t;
};

std::ostream& operator<<(std::ostream& o, const Point& p) {
    o << p.x << "," << p.y << "," << p.z << "," << p.t;

    return o;
}

int distance(const Point& a, const Point& b) {
    return std::abs(a.x - b.x) + std::abs(a.y - b.y) + std::abs(a.z - b.z) + std::abs(a.t - b.t);
}

std::set<Point> add(const Point& p, const std::vector<Point>& points, std::vector<bool>& visited) {
    std::set<Point> result;

    for (int i = 0; i < points.size(); ++i) {
        if (!visited[i] && distance(p, points[i]) <= 3) {
            result.insert(points[i]);
            visited[i] = true;
        }
    }

    return result;
}

bool belongs(const Point& p, const std::set<Point>& cons) {
    for (auto &c: cons) {
        if (distance(c, p) <= 3) {
            return true;
        }
    }

    return false;
}

bool operator<(const Point& a, const Point& b) {
    return a.x < b.x || 
           (a.x == b.x && a.y < b.y) ||
           (a.x == b.x && a.y == b.y && a.z < b.z) ||
           (a.x == b.x && a.y == b.y && a.z == b.z && a.t < b.t);
}

int main(int argc, char* argv[]) {
    std::fstream f("input" + std::string(argv[1]) + ".txt");
    std::vector<Point> points;
    std::vector<bool> visited;
    std::vector<std::set<Point>> constellations;
    std::string token;

    while (getline(f, token, ',')) {
        Point p{std::stoi(token), 0, 0, 0};
        getline(f, token, ',');
        p.y = std::stoi(token);
        getline(f, token, ',');
        p.z = std::stoi(token);
        getline(f, token);
        p.t = std::stoi(token);
        points.push_back(p);
        visited.push_back(false);
    }

    for (int i = 0; i < points.size(); ++i) {
        if (visited[i]) {
            continue;
        } 

        constellations.push_back(std::set<Point>{points[i]});
        std::queue<Point> q;
        q.push(points[i]);

        while (!q.empty()) {
            Point top = q.front();
            q.pop();

            std::set<Point> tba = add(top, points, visited);

            for (auto &p: tba) {
                q.push(p);
                constellations.rbegin()->insert(p);
            }
        }
    }

    std::cout << constellations.size() << std::endl;

    return 0;
}