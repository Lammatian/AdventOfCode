#include <iostream>
#include <vector>
#include <map>
#include <fstream>
#include <string>

int main() {
    std::fstream f("input.txt");
    std::ofstream o("output.txt");
    std::string token;
    int added = 150;
    int gens = 112;

    getline(f, token);
    std::string plants = std::string(added, '.') + token.substr(15) + std::string(added, '.');
    getline(f, token);

    std::map<std::string, std::string> rules;

    for (int i = 0; i < 32; ++i) {
        getline(f, token);
        rules[token.substr(0, 5)] = *token.rbegin();
    }

    for (int i = 0; i < gens; ++i) {
        std::string newPlants;
        newPlants += rules[".." + plants.substr(0, 3)];
        newPlants += rules["." + plants.substr(0, 4)];

        for (int j = 0; j < plants.length() - 4; ++j) {
            newPlants += rules[plants.substr(j, 5)];
        }

        newPlants += rules[plants.substr(plants.length() - 4) + "."];
        newPlants += rules[plants.substr(plants.length() - 3) + ".."];
        plants = newPlants;
        o << plants << std::endl;
    }

    std::cout << plants << std::endl;

    int result = 0;

    for (int i = 0; i < plants.length(); ++i) {
        if (plants[i] == '#') {
            result += i - added;
        }
    }

    std::cout << result << std::endl;

    return 0;
}