#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>

std::pair<int, int> countTwoThree(std::string input) {
    std::pair<int, int> result;
    std::unordered_map<char, int> count;

    for (auto &c: input) {
        count[c]++;

        if (count[c] == 2) {
            result.first++;
        } else if (count[c] == 3) {
            result.first--;
            result.second++;
        }
    }

    return result;
}

int main() {
    std::fstream f("input.txt"); 
    int twos, threes;

    std::string token;

    while (getline(f, token)) {
        std::pair<int, int> count = countTwoThree(token);

        twos += count.first > 0 ? 1 : 0;
        threes += count.second > 0 ? 1 : 0;
    }

    std::cout << twos * threes << std::endl;

    return 0;
}