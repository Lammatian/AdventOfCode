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
        ll num;
        std::vector<ll> result;

        while (f >> num) {
            result.push_back(num);
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

    std::vector<std::string> split(std::string s, std::string delim) {
        std::vector<std::string> result;
        size_t last = 0;
        size_t next = 0;

        while (s.find(delim, last) != std::string::npos) {
            next = s.find(delim, last);
            result.push_back(s.substr(last, next - last));
            last = next + delim.size();
        }

        result.push_back(s.substr(last));

        return result;
    }

    std::vector<std::string> split(std::string s, std::vector<std::string> delims) {
        std::vector<std::string> result;
        size_t last = 0;
        size_t next = 0;
        int delim_idx = 0;

        while (s.find(delims[delim_idx], last) != std::string::npos) {
            next = s.find(delims[delim_idx], last);
            result.push_back(s.substr(last, next - last));
            last = next + 1;
            delim_idx++;
        }

        result.push_back(s.substr(last));
        return result;
    }
}