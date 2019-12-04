#include <iostream>
#include <vector>
#include <fstream>
#include <queue>
#include <stack>
#include <cmath>
#include <unistd.h>

struct Point {
    int x;
    int y;
};

int main(int argc, char* argv[]) {
    std::ifstream f("input" + std::string(argv[1]) + ".txt");
    std::ofstream o("output.txt");
    int minx = 2000;
    int maxx = 0;
    int maxy = 0;
    int topclay = 2000;
    std::string token;

    while (getline(f, token, '=')) {
        bool startx = token == "x";
        getline(f, token, ',');

        if (startx) {
            maxx = std::max(std::stoi(token), maxx);
            minx = std::min(std::stoi(token), minx);
        } else {
            maxy = std::max(std::stoi(token), maxy);
            topclay = std::min(std::stoi(token), topclay);
        }

        getline(f, token, '=');
        getline(f, token, '.');

        if (!startx) {
            minx = std::min(std::stoi(token), minx);
        } else {
            topclay = std::min(std::stoi(token), topclay);
        }

        getline(f, token, '.');
        getline(f, token);

        if (startx) {
            maxy = std::max(std::stoi(token), maxy);
        } else {
            maxx = std::max(std::stoi(token), maxx);
        }
    }

    std::vector<std::string> board(maxy + 2);

    for (int i = 0; i < maxy + 2; ++i) {
        board[i] = std::string(maxx + 2, '.');
    }

    f.clear();
    f.seekg(0);

    while (getline(f, token, '=')) {
        int x = 0;
        int y = 0;
        bool startx = token == "x";

        getline(f, token, ',');

        if (startx) {
            x = std::stoi(token);
        } else {
            y = std::stoi(token);
        }

        if (y > maxy) {
            getline(f, token);
            continue;
        }

        getline(f, token, '=');
        getline(f, token, '.');
        int start = std::stoi(token);
        getline(f, token, '.');
        getline(f, token);
        int end = std::stoi(token);

        if (startx && end > maxy) {
            continue;
        }

        if (startx) {
            for (int i = start; i <= end; ++i) {
                board[i][x] = '#';
            }
        } else {
            board[y] = board[y].substr(0, start) + std::string(end - start + 1, '#') + board[y].substr(end + 1);
        }
    }

    for (auto &s: board) {
        std::cout << s.substr(minx - 1) << std::endl;
    }
    std::cout << std::endl;

    std::stack<Point> q;
    q.push({500, 0});

    int water = 0;
    int count = 0;

    while (!q.empty()) {
        Point top = q.top();
        q.pop();

        if (board[top.y][top.x] == 'w' || board[top.y][top.x] == '#') {
            continue;
        }

        while (top.y <= maxy && board[top.y + 1][top.x] != '#') {
            // mark water
            board[top.y][top.x] = 'w';
            // travel down
            top.y++;

            if (top.y > maxy) {
                break;
            }
        }

        if (top.y > maxy) {
            continue;
        }

        board[top.y][top.x] = 'w';

        // check left and right 
        Point right = {top.x, top.y};
        Point left = {top.x, top.y};
        bool wallr = false;
        bool walll = false;

        while (board[right.y][right.x + 1] != '#' && board[right.y + 1][right.x] == '#') {
            // mark water
            board[right.y][right.x] = 'w';
            right.x++;
        }

        wallr = board[right.y + 1][right.x] == '#';

        while (board[left.y][left.x - 1] != '#' && board[left.y + 1][left.x] == '#') {
            // mark water
            board[left.y][left.x] = 'w';
            left.x--;
        }

        walll = board[left.y + 1][left.x] == '#';

        if (wallr && walll) {
            // still water, may treat as sand
            int lastwater = water;
            for (int x = left.x; x <= right.x; ++x) {
                board[left.y][x] = '#';
                water++;
            }

            top.y--;
            board[top.y][top.x] = 'x';
            q.push(top);

            continue;
        }

        if (!wallr) {
            q.push(right);
        } else {
            board[right.y][right.x] = 'w';
        }

        if (!walll) {
            q.push(left);
        } else {
            board[left.y][left.x] = 'w';
        }
    }

    int running = 0;

    for (int i = topclay; i < board.size(); ++i) {
        std::string s = board[i];
        std::cout << s.substr(minx - 1) << std::endl;
        for (auto &c: s) {
            if (c == 'w') {
                running++;
            }
        }
    }

    for (auto &s: board) {
        o << s.substr(minx - 1) << std::endl;
    }

    std::cout << "Still: " << water << std::endl;
    std::cout << "Moving: " << running << std::endl;
    std::cout << "Total: " << water + running << std::endl;

    return 0;
}