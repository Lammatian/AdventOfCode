#include <iostream>
#include <fstream>
#include <set>
#include <map>
#include <algorithm>

#include "util.h"

typedef std::map<char, ll>::value_type char_count;

struct group {
    std::string responses;
    int response_count;
};

std::vector<group> prep(const std::vector<std::string>& lines) {
    std::vector<group> result;
    std::string current = "";
    int count = 0;

    for (auto& line: lines) {
        if (line == "") {
            result.push_back({current, count});
            current = "";
            count = 0;
        } else {
            current += line;
            count += 1;
        }
    }

    if (current != "") {
        result.push_back({current, count});
    }

    return result;
}

ll sol1(std::vector<group> groups) {
    ll result = 0;

    for (auto& group: groups) {
        std::set<char> s(group.responses.begin(), group.responses.end());
        result += s.size();
    }

    return result;
}

ll sol2(std::vector<group> groups) {
    ll result = 0;

    for (auto& group: groups) {
        std::vector<char> resp(group.responses.begin(), group.responses.end());
        std::map<char, ll> count = util::count(resp);

        // Count only those answers who appear in all responses
        result += std::count_if(count.begin(), count.end(),
                                [&group](char_count cc) {
                                    return cc.second == group.response_count;
                                });
    }

    return result;
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<group> groups = prep(util::readlines(f));

    std::cout << sol1(groups) << std::endl;
    std::cout << sol2(groups) << std::endl;

    return 0;
}