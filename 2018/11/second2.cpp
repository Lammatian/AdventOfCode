#include <iostream>
#include <vector>

struct result {
    std::pair<int, int> point;
    int value;
};

int INPUT = 3628;
int SIZE = 300;

void computePower(std::vector<std::vector<int>>& p, int s) {
    for (int y = 1; y <= SIZE; ++y) {
        for (int x = 1; x <= SIZE; ++x) {
            p[y][x] = (((y * (x + 10) + s) * (x + 10) / 100) % 10) - 5;
        }
    }
}

result findMax(std::vector<std::vector<std::vector<int>>>& ps, const std::vector<std::vector<int>> f, int s) {
    int curr = 0;
    int _max = 0;
    std::pair<int, int> maxPt = std::make_pair(1, 1);

    for (int y = 1; y <= SIZE - s + 1; ++y) {
        for (int x = 1; x <= SIZE - s + 1; ++x) {
            curr = ps[s - 1][y][x];

            for (int i = y; i < y + s; ++i) {
                curr += f[i][x + s - 1];
            }

            for (int i = x; i < x + s; ++i) {
                curr += f[y + s - 1][i];
            }

            curr -= f[y + s - 1][x + s - 1];
            ps[s][y][x] = curr;

            if (curr > _max) {
                _max = curr;
                maxPt = std::make_pair(x, y);
            }
        }
    }

    return {maxPt, _max};
}

int main() {
    std::vector<std::vector<int>> power(SIZE + 1);
    std::vector<std::vector<std::vector<int>>> powers(SIZE + 1);

    for (int i = 0; i < SIZE + 1; ++i) {
        power[i] = std::vector<int>(SIZE + 1);
    }

    for (int i = 0; i < SIZE + 1; ++i) {
        powers[i] = std::vector<std::vector<int>>(SIZE + 2 - i);

        for (int j = 0; j < SIZE + 2 - i; ++j) {
            powers[i][j] = std::vector<int>(SIZE + 2 - i);
        }
    }

    computePower(power, INPUT);
    powers[1] = power;
    result best = {std::make_pair(0, 0), 0};
    int best_size = 0;

    for (int i = 2; i < 300; ++i) {
        result current_best = findMax(powers, power, i);
        if (current_best.value > best.value) {
            best = current_best;
            best_size = i;
        }
    }

    std::cout << best.point.first << "," << best.point.second << "," << best_size << std::endl;

    return 0;
}
