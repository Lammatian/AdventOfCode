#include <fstream>
#include <vector>

typedef long long ll;

std::vector<std::string> readlines(std::ifstream& f) {
    std::string line;
    std::vector<std::string> result;

    while (std::getline(f, line)) {
        result.push_back(line);
    }

    return result;
}

std::vector<ll> readlls(std::ifstream& f) {
    std::string line;
    std::vector<ll> result;

    while (std::getline(f, line)) {
        result.push_back(std::stoll(line));
    }

    return result;
}