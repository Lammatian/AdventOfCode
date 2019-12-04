#include <iostream>
#include <vector>

int INPUT = 3628;
int SIZE = 300;

void computePower(std::vector<std::vector<int>>& p, std::vector<std::vector<int>>& pt, int s) {
    for (int y = 1; y <= SIZE; ++y) {
        for (int x = 1; x <= SIZE; ++x) {
            p[y][x] = (((y * (x + 10) + s) * (x + 10) / 100) % 10) - 5;
            pt[y][x] = p[y][x] + pt[y - 1][x] + pt[y][x - 1] - pt[y - 1][x - 1];
        }
    }
}

int main () {
    std::vector<std::vector<int>> partial(SIZE + 1);
    std::vector<std::vector<int>> power(SIZE + 1);

    for (int i = 0; i < SIZE + 1; ++i) {
        power[i] = std::vector<int>(SIZE + 1);
        partial[i] = std::vector<int>(SIZE + 1);
    }

    computePower(power, partial, INPUT);
    std::pair<int, int> bestPt = std::make_pair(0, 0);
    int bestValue = 0;
    int bestSize = 0;

    for (int s = 1; s <= SIZE; ++s) {
        for (int y = s; y <= SIZE - s + 1; ++y) {
            for (int x = s; x <= SIZE - s + 1; ++x) {
                int current = partial[y][x] - partial[y - s][x] - partial[y][x - s] + partial[y - s][x - s];
                
                if (current > bestValue) {
                    bestPt = std::make_pair(x - s + 1, y - s + 1);
                    bestValue = current;
                    bestSize = s;
                }
            }
        }
    }

    std::cout << bestPt.first << "," << bestPt.second << "," << bestSize << std::endl;

    return 0;
}