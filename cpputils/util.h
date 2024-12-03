#include <fstream>
#include <vector>
#include <map>
#include <set>
#include <regex>
#include <sstream>
#include <numeric>

typedef long long ll;

namespace util {
    // TODO: Testing?

    /**
     * Read lines from an std::ifstream and return as strings
     */
    std::vector<std::string> readlines(std::ifstream& f) {
        std::string line;
        std::vector<std::string> result;

        while (std::getline(f, line)) {
            result.push_back(line);
        }

        return result;
    }

    /**
     * Read long long numbers from an std::ifstream
     */
    std::vector<ll> readlls(std::ifstream& f) {
        ll num;
        std::vector<ll> result;

        while (f >> num) {
            result.push_back(num);
        }

        return result;
    }

    /**
     * Count the number of occurrences of each element in a vector
     * Very limited version of Python's collections.Counter
     **/
    template <typename T, typename A>
    std::map<T, ll> count(const std::vector<T, A>& vals) {
        std::map<T, ll> result;

        for (auto& v: vals) {
            if (result.find(v) != result.end()) {
                result[v]++;
            } else {
                result[v] = 1;
            }
        }

        return result;
    }

    /**
     * Tokenize a string based on a series of delimiters.
     * Returns a vector of tokens, one per delimiter (+ one at end)
     * e.g tokenize("a:b;c,d", {':', ';', ','}) -> {"a", "b", "c", "d"}
     * If there are too many delimiters, stops at EOF
     **/
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

    /**
     * Split a string into parts that are delimited by `delim`. Finds all the
     * occurrences of delim.
     * e.g. tokenize("a,b,c,d", ',') -> {"a", "b", "c", "d"}
     **/
    std::vector<std::string> tokenize(std::string s, char delim) {
        std::stringstream ss(s);
        std::string token;
        std::vector<std::string> result;

        while (getline(ss, token, delim)) {
            result.push_back(token);
        }

        return result;
    }

    /**
     * Split a string into parts that are delimited by the string `delim`.
     * e.g. split("a,,b,,c", ",,") -> {"a", "b", "c"}
     **/
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

    /**
     * Split a string into parts that are delimited by `delims`.
     * e.g. split("a,,b..c", {",,", ".."}) -> {"a", "b", "c"}
     **/
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

    
    /**
     * In-place intersection as there is none in STL lol
     **/
    template<typename T>
    void intersection_ip(std::set<T>& target, const std::set<T>& other) {
        auto it1 = target.begin();
        auto it2 = other.begin();

        while (it1 != target.end() && it2 != other.end()) {
            if (*it1 < *it2) {
                target.erase(it1++);
            } else if (*it2 < *it1) {
                ++it2;
            } else {
                // *it1 == *it2
                ++it1;
                ++it2;
            }
        }

        target.erase(it1, target.end());
    }

    /**
     * Not in-place intersection that is just quicker to write
     **/
    template<typename T>
    std::set<T> intersection(const std::set<T>& first, const std::set<T>& second) {
        std::set<T> result;
        auto it1 = first.begin();
        auto it2 = second.begin();

        while (it1 != first.end() && it2 != second.end()) {
            if (*it1 < *it2) {
                ++it1;
            } else if (*it2 < *it1) {
                ++it2;
            } else {
                // *it1 == *it2
                result.emplace(*it1);
                ++it1;
                ++it2;
            }
        }

        return result;
    }

    /** 
     * Join a string using a delimiter
     * e.g. join({"a", "b", "c"}, ",") -> "a,b,c"
     */
    std::string join(const std::vector<std::string>& v, const std::string& delim) {
        auto fold = [&delim](std::string a, std::string b) {
            return std::move(a) + delim + std::move(b);
        };
        return std::accumulate(std::next(v.begin()), v.end(), v[0], fold);
    }

    /**
     * Given a string and a regex, get all the matches for that regex
     */
    std::vector<std::smatch> find_all(const std::string& s, const std::regex& r) {
        std::vector<std::smatch> result;
        std::smatch match;
        std::string::const_iterator searchStart(s.cbegin());
        while (regex_search(searchStart, s.cend(), match, r)) {
            result.push_back(match);
            searchStart = match.suffix().first;
        }
        return result; 
    }
}
