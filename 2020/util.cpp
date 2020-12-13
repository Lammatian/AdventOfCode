#include <sstream>

#include "util.h"

namespace util {
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

    std::vector<std::string> tokenize(std::string s,
                                      std::vector<char> delims) {
        std::stringstream ss(s);
        std::string token;
        std::vector<std::string> result;
        int idx = 0;

        // to capture the end of the string
        delims.push_back('\n');

        for (auto& d : delims) {
            if (ss.eof()) {
                return result;
            }

            getline(ss, token, d);
            result.push_back(token);
        }

        return result;
    }

    std::vector<std::string> tokenize(std::string s, char delim) {
        std::stringstream ss(s);
        std::string token;
        std::vector<std::string> result;

        while (getline(ss, token, delim)) {
            result.push_back(token);
        }

        return result;
    }
}