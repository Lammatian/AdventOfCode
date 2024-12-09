#include <fstream>
#include <vector>
#include <map>
#include <set>
#include <regex>
#include <sstream>
#include <numeric>
#include <iostream>

typedef long long ll;

template<typename T>
std::ostream& operator<<(std::ostream& o, const std::vector<T>& v) {
    if (v.empty()) return o << "[]";
    o << "[";
    for (size_t i = 0; i < v.size() - 1; ++i) {
        o << v[i] << ", "; 
    }
    return o << v.back() << "]";
}

template<typename T>
std::ostream& operator<<(std::ostream& o, const std::set<T>& s) {
    if (s.empty()) return o << "{}";
    o << "{";
    auto it = s.begin();
    o << *it;
    it++;
    for (; it != s.end(); ++it) {
        o << ", " << *it;
    }
    return o << "}";
}

template<typename K, typename V>
std::ostream& operator<<(std::ostream& o, const std::map<K, V>& m) {
    if (m.empty()) return o << "{}";
    o << "{";
    for (const auto& [k, v]: m) {
        o << "\n  " << k << ": " << v;
    }
    return o << "\n}";
}

namespace util {
    // TODO: Testing?

    /**
     * Structure to represent a position on a 2D grid
     */
    struct pos {
        int r;
        int c;

        pos& operator+=(pos other) {
            r += other.r;
            c += other.c;
            return *this; 
        }

        pos& operator-=(pos other) {
            r -= other.r;
            c -= other.c;
            return *this; 
        }

        friend pos operator+(pos p1, pos p2) {
            p1 += p2;
            return p1;
        }

        friend pos operator-(pos p1, pos p2) {
            p1 -= p2;
            return p1;
        }

        friend bool operator<(pos p1, pos p2) {
            return p1.r < p2.r || (p1.r == p2.r && p1.c < p2.c);
        }

        friend std::ostream& operator<<(std::ostream& o, pos p) {
            return o << "(" << p.r << ", " << p.c << ")";
        }

        friend bool in_bounds(pos p, int maxr, int maxc) {
            return p.r >= 0 && p.r < maxr && p.c >= 0 && p.c < maxc;
        }
    };

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
