#include <iostream>
#include <fstream>
#include <vector>

#include "util.h"

struct pos {
    int x;
    int y;
};

ll traverse(const std::vector<std::string>& lines,
            int right,
            int bottom) {
    pos p = {0, 0};    
    int width = lines[0].size();
    ll result = 0;

    while (p.y < lines.size()) {
        if (lines[p.y][p.x] == '#') {
            result++;
        }

        p.x = (p.x + right) % width;
        p.y += bottom;
    }

    return result;
}

ll sol1(const std::vector<std::string>& lines) {
    return traverse(lines, 3, 1);
}

ll sol2(const std::vector<std::string>& lines) {
    ll result = 1;
    std::vector<pos> moves = {{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}};

    for (pos& p : moves) {
        result *= traverse(lines, p.x, p.y);
    }

    return result;
}

int main() {
    std::ifstream f("input.txt");
    std::vector<std::string> lines = util::readlines(f);

    std::cout << sol1(lines) << std::endl;
    std::cout << sol2(lines) << std::endl;

    return 0;
}