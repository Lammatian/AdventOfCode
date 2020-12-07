#include <iostream>
#include <fstream>
#include <regex>
#include <map>
#include <set>
#include <queue>

#include "util.h"

// Assume at most 10 bags contained in one
std::vector<char> DELIMS(10, ',');

struct containee {
    std::string name;
    int quantity;
};

struct rule {
    std::string container;   
    std::vector<containee> containees;
};

std::vector<rule> prep(std::vector<std::string> lines) {
    std::vector<rule> result;
    std::regex full_re ("^(\\w+ \\w+) bags contain((?: \\d \\w+ \\w+ bags?[,\\.])+| no other bags.)$");
    std::regex containee_re ("^ (\\d) (\\w+ \\w+) bags?$");
    std::smatch matches;

    for (auto& line: lines) {
        std::regex_search(line, matches, full_re);

        std::string container = matches[1];
        std::string containees_str = matches[2];

        if (containees_str == " no other bags.") {
            result.push_back({container, {}});
            continue;
        }

        // Remove . at end
        containees_str.pop_back();

        std::vector<std::string> containees_unparsed = util::tokenize(containees_str, DELIMS);
        std::vector<containee> containees;

        for (auto& cu: containees_unparsed) {
            std::regex_search(cu, matches, containee_re);
            containees.push_back({matches[2], std::stoi(matches[1])});
        }

        result.push_back({container, containees});
    }

    return result;
}

ll sol1(std::vector<rule> rules) {
    // Assumes no loops
    std::set<std::string> shiny_gold_ancestors;
    std::map<std::string, std::vector<std::string>> parents;

    for (auto& rule: rules) {
        for (auto& containee: rule.containees) {
            parents[containee.name].push_back(rule.container);
        }
    }

    // BFS
    std::queue<std::string> q;
    q.push("shiny gold");

    while (!q.empty()) {
        std::string cur = q.front();
        q.pop();

        if (parents.find(cur) == parents.end()) {
            continue;
        }

        for (auto& parent: parents[cur]) {
            q.push(parent);
            shiny_gold_ancestors.insert(parents[cur].begin(), parents[cur].end());
        }
    }

    return shiny_gold_ancestors.size();
}

ll sol2(std::vector<rule> rules) {
    ll result = 0;
    std::map<std::string, std::vector<containee>> children;

    for (auto& rule: rules) {
        children[rule.container] = rule.containees;
    }

    std::queue<std::pair<std::string, int>> q;
    q.push({"shiny gold", 1});

    while (!q.empty()) {
        std::string container = q.front().first;
        int quantity = q.front().second;
        q.pop();

        result += quantity;

        std::for_each(children[container].begin(), children[container].end(),
                      [&q, &quantity](containee c) {
                          q.push({c.name, quantity * c.quantity});
                      });
    }

    // We do not count shiny gold as one of the bags
    return result - 1;
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<rule> lines = prep(util::readlines(f));

    std::cout << sol1(lines) << std::endl;
    std::cout << sol2(lines) << std::endl;

    return 0;
}