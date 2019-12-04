#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
    std::ifstream f("input.txt");
    std::ofstream g("output.txt");
    std::vector<std::vector<int>> fabric(1000);

    for (int i = 0; i < 1000; ++i) {
        fabric[i] = std::vector<int>(1000);
    }

    std::string token;
    int result = 0;

    while (f.peek() != EOF) {
        getline(f, token, '@');
        getline(f, token, ',');
        int x = std::stoi(token);
        getline(f, token, ':');
        int y = std::stoi(token);
        getline(f, token, 'x');
        int w = std::stoi(token);
        getline(f, token);
        int h = std::stoi(token);

        for (int r = y; r < y + h; ++r) {
            for (int c = x; c < x + w; ++c) {
                fabric[r][c]++;

                if (fabric[r][c] == 2) {
                    result++;
                }
            }
        }
    }

    for (int i = 0; i < 1000; ++i) {
        for (int j = 0; j < 1000; ++j) {
            g << fabric[i][j];
        }

        g << std::endl;
    }

    std::cout << result << std::endl;

    return 0;
}