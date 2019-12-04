#include <iostream>
#include <vector>

struct result {
    std::pair<int, int> point;
    int value;
};

int INPUT = 3628;

void computePower(std::vector<std::vector<int>>& p, int s) {
    for (int y = 1; y <= 300; ++y) {
        for (int x = 1; x <= 300; ++x) {
            p[y][x] = (((y * (x + 10) + s) * (x + 10) / 100) % 10) - 5;
        }
    }
}

result findMax(std::vector<std::vector<int>>& p, int s) {
    int curr = 0;
    int _max = 0;
    std::pair<int, int> maxPt = std::make_pair(1, 1);

    for (int y = 1; y <= 300 - s + 1; ++y) {
        curr = 0;

        for (int i = y; i < y + s; ++i) {
            for (int j = 1; j <= s - 1; ++j) {
                curr += p[i][j];
            }
        }
        
        for (int x = 1; x <= 300 - s + 1; ++x) {
            for (int i = y; i < y + s; ++i) {
                curr -= p[i][x - 1];
                curr += p[i][x + s - 1];
            }

            if (curr > _max) {
                _max = curr;
                maxPt = std::make_pair(x, y);
            }
        }
    }

    return {maxPt, _max};
}

int main() {
    std::vector<std::vector<int>> power(301);

    for (int i = 0; i < 301; ++i) {
        power[i] = std::vector<int>(301);
    }

    computePower(power, INPUT);
    result best = {std::make_pair(0, 0), 0};
    int best_size = 0;

    for (int i = 1; i < 300; ++i) {
        result current_best = findMax(power, i);
        if (current_best.value > best.value) {
            best = current_best;
            best_size = i;
        }
    }

    std::cout << best.point.first << "," << best.point.second << "," << best_size << std::endl;

    return 0;
}
