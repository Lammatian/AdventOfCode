#include <iostream>
#include <vector>

int INPUT = 3628;

void computePower(std::vector<std::vector<int>>& p, int s) {
    for (int y = 1; y <= 300; ++y) {
        for (int x = 1; x <= 300; ++x) {
            p[y][x] = (((y * (x + 10) + s) * (x + 10) / 100) % 10) - 5;
        }
    }
}

std::pair<int, int> findMax(std::vector<std::vector<int>>& p) {
    int curr = 0;
    int _max = 0;
    std::pair<int, int> maxPt = std::make_pair(1, 1);

    for (int y = 1; y <= 298; ++y) {
        curr = 0;

        for (int i = y; i < y + 3; ++i) {
            for (int j = 1; j <= 2; ++j) {
                curr += p[i][j];
            }
        }
        
        for (int x = 1; x <= 298; ++x) {
            for (int i = y; i < y + 3; ++i) {
                curr -= p[i][x - 1];
                curr += p[i][x + 2];
            }

            if (curr > _max) {
                _max = curr;
                maxPt = std::make_pair(x, y);
            }
        }
    }

    return maxPt;
}

int main() {
    std::vector<std::vector<int>> power(301);

    for (int i = 0; i < 301; ++i) {
        power[i] = std::vector<int>(301);
    }

    computePower(power, INPUT);
    std::pair<int, int> best = findMax(power);

    std::cout << "(" << best.first << ", " << best.second << ")" << std::endl;

    return 0;
}