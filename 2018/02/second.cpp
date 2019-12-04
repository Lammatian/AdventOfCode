#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::pair<int, std::string> difference(std::string a, std::string b) {
    std::pair<int, std::string> result;

    for (int i = 0; i < a.length(); ++i) {
        if (a[i] != b[i]) {
            result.first++;
        } else {
            result.second += a[i];
        }
    }

    return result;
}

int main() {
    std::fstream f("input.txt");
    std::vector<std::string> boxes;
    std::string token;

    while (getline(f, token)) {
        boxes.push_back(token);
    }

    for (int i = 0; i < boxes.size(); ++i) {
        for (int j = i + 1; j < boxes.size(); ++j) {
            std::pair<int, std::string> diff = difference(boxes[i], boxes[j]);
            if (diff.first == 1) {
                std::cout << diff.second << std::endl;
                return 0;
            }
        }
    }

    return 0;
}