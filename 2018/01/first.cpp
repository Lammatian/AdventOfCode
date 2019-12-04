#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream f("input.txt");

    int sum = 0;
    std::string curr;

    while (std::getline(f, curr)) {
        sum += std::stoi(curr);
    }

    std::cout << sum << std::endl;

    return 0;
}