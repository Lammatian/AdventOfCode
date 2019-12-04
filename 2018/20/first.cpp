#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <queue>
#include <stack>
#include <map>
#include <cmath>

struct Point {
    int x;
    int y;

    Point& operator +=(const Point& p) {
        x += p.x;
        y += p.y;
    }
};

bool operator<(const Point& a, const Point& b) {
    return a.x < b.x || (a.x == b.x && a.y < b.y);
}

Point operator+(const Point& a, const Point& b) {
    return {a.x + b.x, a.y + b.y};
}

std::map<char, Point> dirs = {
    {'N', {0, 1}},
    {'E', {1, 0}},
    {'S', {0, -1}},
    {'W', {-1, 0}}
};

struct Room {
    std::map<char, bool> doors = {
        {'N', false},
        {'E', false},
        {'S', false},
        {'W', false}
    };
};

std::set<Point> search(std::fstream& f, std::vector<std::vector<Room>>& rooms, std::set<Point> leaves, int& maxx, int& maxy, int& minx, int& miny, char end) {
    std::set<Point> newLeaves; // leaves from all branches
    std::vector<Point> currLeaves(leaves.begin(), leaves.end()); // leaves from current branch
    char next;

    while (f.get(next)) {
        if (next == end) {
            // add leaves from the last branch and return
            newLeaves.insert(currLeaves.begin(), currLeaves.end());
            return newLeaves;
        }

        if (next == '(') {
            // branching point
            std::set<Point> s = search(f, rooms, std::set<Point>(currLeaves.begin(), currLeaves.end()), maxx, maxy, minx, miny, ')');
            currLeaves = std::vector<Point>(s.begin(), s.end());
        } else if (next == '|') {
            // begin new branch
            newLeaves.insert(currLeaves.begin(), currLeaves.end());
            // branch begins with old leaves
            currLeaves = std::vector<Point>(leaves.begin(), leaves.end());
        } else { // directions
            // perform movement
            for (auto it = currLeaves.begin(); it != currLeaves.end(); ++it) {
                rooms[it->y][it->x].doors[next] = true;
                it->x += dirs[next].x;
                it->y += dirs[next].y;
                maxx = std::max(maxx, it->x);
                maxy = std::max(maxy, it->y);
                minx = std::min(minx, it->x);
                miny = std::min(miny, it->y);
            }
        }
    }
}

int furthest(std::vector<std::vector<Room>> rooms, Point start) {
    std::queue<std::pair<Point, int>> q;
    std::vector<std::vector<bool>> visited(rooms.size());
    for (auto &v: visited) {
        v = std::vector<bool>(rooms[0].size());
    }
    q.push(std::make_pair(start, 0));
    int maxd = 0;

    while (!q.empty()) {
        std::pair<Point, int> top = q.front();
        maxd = std::max(maxd, top.second);
        q.pop();

        for (auto &d: rooms[top.first.y][top.first.x].doors) {
            Point newPoint = top.first + dirs[d.first];
            if (d.second && !visited[newPoint.y][newPoint.x]) {
                visited[newPoint.y][newPoint.x] = true;
                q.push(std::make_pair(top.first + dirs[d.first], top.second + 1));
            }
        }
    }

    return maxd;
}

int main(int argc, char* argv[]) {
    std::fstream f("input" + std::string(argv[1]) + ".txt");   
    std::vector<std::vector<Room>> rooms(101);

    for (auto& row: rooms) {
        row = std::vector<Room>(101);
    }

    f.seekg(1); // Ignore ^
    int maxx = 0;
    int maxy = 0;
    int minx = 0;
    int miny = 0;

    search(f, rooms, {{52, 50}}, maxx, maxy, minx, miny, '$');

    std::cout << maxx << ", " << maxy << std::endl;
    std::cout << minx << ", " << miny << std::endl;
    std::cout << furthest(rooms, {52, 50}) << std::endl;

    return 0;
}