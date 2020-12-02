#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>

#include "util.h"

struct policy {
    ll first;
    ll second;
    char c;

    /* 
     * Constructor for the policy given a string, assumes a string
     * of the form "<FIRST>-<SECOND> <CHAR>"
     */
    policy(std::string s) {
        auto tokens = util::tokenize(s, {'-', ' '});

        first = std::stoll(tokens[0]);
        second = std::stoll(tokens[1]);
        c = tokens[2].at(0);
    }
};

bool satisfies_count_policy(std::string password, policy p) {
    ll occs = std::count(password.begin(), password.end(), p.c);
    return p.first <= occs && occs <= p.second;
}

ll sol1(std::vector<std::string> lines) {
    ll result = 0;

    for (auto& line : lines) {
        auto tokens = util::tokenize(line, {':'});

        policy p = policy(tokens[0]);
        // Have to remove the leading whitespace
        std::string password = tokens[1].substr(1);

        if (satisfies_count_policy(password, p)) {
            result++;
        }
    }

    return result;
}

bool satisfies_position_policy(std::string password, policy p) {
    return (password.at(p.first - 1) == p.c) !=
           (password.at(p.second - 1) == p.c);
}

ll sol2(std::vector<std::string> lines) {
    ll result = 0;

    for (auto& line : lines) {
        auto tokens = util::tokenize(line, {':'});

        policy p = policy(tokens[0]);
        // Have to remove the leading whitespace
        std::string password = tokens[1].substr(1);

        if (satisfies_position_policy(password, p)) {
            result++;
        }
    }

    return result;
}

int main() {
    std::ifstream f("input.txt");

    std::vector<std::string> lines = util::readlines(f);

    std::cout << sol1(lines) << std::endl;
    std::cout << sol2(lines) << std::endl;

    return 0;
}
