#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct Point {
    int x;
    int y;
};

int main(int argc, char* argv[]) {
    Point target = {10, 785};
    int depth = 5616;

    std::vector<std::vector<int>> risks(target.y + 1);

    for (auto &r: risks) {
        r = std::vector<int>(target.x + 1);
    }

    int result = 0;

    for (int y = 0; y <= target.y; ++y) {
        for (int x = 0; x <= target.x; ++x) {
            if (x == 0) {
                risks[y][x] = (48271*y + depth) % 20183;
            } else if (y == 0) {
                risks[y][x] = (16807*x + depth) % 20183;
            } else {
                risks[y][x] = (risks[y][x - 1] * risks[y - 1][x] + depth) % 20183;
            }
        }
    }

    for (int y = 0; y <= target.y; ++y) {
        for (int x = 0; x <= target.x; ++x) {
            result += risks[y][x] % 3;
        }
    }

    for (auto &row: risks) {
        for (auto &r: row) {
            std::cout << r%3 << " ";
        }
        std::cout << std::endl;
    }

    std::cout << result << std::endl;

    return 0;
}