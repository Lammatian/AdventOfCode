#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

int main() {
    std::unordered_set<int> count;
    std::ifstream f("input.txt");

    int sum = 0;

    while (true) {
        std::string tok;

        while (getline(f, tok)) {
            sum += std::stoi(tok);

            if (count.find(sum) == count.end()) {
                count.insert(sum);
            } else {
                std::cout << sum << std::endl;
                return 0;
            }
        }

        f.clear();
        f.seekg(0, f.beg);
    }
    return 0;
}