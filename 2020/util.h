#include <fstream>
#include <vector>
#include <map>

typedef long long ll;

namespace util {
    // TODO: Testing?

    std::vector<std::string> readlines(std::ifstream& f);

    std::vector<ll> readlls(std::ifstream& f);

    /*
     * Count the number of occurrences of each element in a vector
     * Very limited version of Python's collections.Counter
     */
    template <typename T>
    std::map<T, ll> count(std::vector<T> vals);

    /*
     * Tokenize a string based on a series of delimiters.
     * Returns a vector of tokens, one per delimiter (+ one at end)
     * e.g tokenize("a:b;c,d", {':', ';', ','}) -> {"a", "b", "c", "d"}
     * If there are too many delimiters, stops at EOF
     */
    // TODO: Version with string delimiters?
    std::vector<std::string> tokenize(std::string s, std::vector<char> delims);
}