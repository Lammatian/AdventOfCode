#include <fstream>
#include <vector>
#include <map>
#include <set>

typedef long long ll;

namespace util {
    // TODO: Testing?

    std::vector<std::string> readlines(std::ifstream& f);

    std::vector<ll> readlls(std::ifstream& f);

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
    // TODO: Version with string delimiters?
    std::vector<std::string> tokenize(std::string s, std::vector<char> delims);

    /**
     * Split a string into parts that are delimited by `delim`. Finds all the
     * occurrences of delim.
     * e.g. tokenize("a,b,c,d", ',') -> {"a", "b", "c", "d"}
     **/
    std::vector<std::string> tokenize(std::string s, char delim);

    /**
     * Split a string into parts that are delimited by the string `delim`.
     * e.g. split("a,,b,,c", ",,") -> {"a", "b", "c"}
     **/
    std::vector<std::string> split(std::string s, std::string delim);

    /**
     * Split a string into parts that are delimited by `delims`.
     * e.g. split("a,,b..c", {",,", ".."}) -> {"a", "b", "c"}
     **/
    std::vector<std::string> split(std::string s, std::vector<std::string> delims);
    
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
}