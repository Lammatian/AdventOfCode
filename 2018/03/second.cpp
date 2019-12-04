#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_set>

int WIDTH = 1000;

int main() {
    std::fstream f("input.txt");
    std::vector<std::vector<std::vector<int>>> fabric(WIDTH);
    std::unordered_set<int> unoverlapped;

    for (int i = 0; i < WIDTH; ++i) {
        fabric[i] = std::vector<std::vector<int>>(WIDTH);
    }

    std::string token;

    while (f.peek() != EOF) {
        getline(f, token, '#');
        getline(f, token, '@');
        int id = std::stoi(token);
        getline(f, token, ',');
        int x = std::stoi(token);
        getline(f, token, ':');
        int y = std::stoi(token);
        getline(f, token, 'x');
        int w = std::stoi(token);
        getline(f, token);
        int h = std::stoi(token);
        bool overlapped = false;

        for (int r = y; r < y + h; ++r) {
            for (int c = x; c < x + w; ++c) {
                if (fabric[r][c].size() == 1) {
                    unoverlapped.erase(fabric[r][c][0]);
                }

                if (fabric[r][c].size() >= 1) {
                    overlapped = true;
                }

                fabric[r][c].push_back(id);
            }
        }

        if (!overlapped) {
            unoverlapped.insert(id);
        }
    }

    std::cout << *unoverlapped.begin() << std::endl;

    return 0;
}
