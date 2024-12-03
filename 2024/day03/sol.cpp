#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <vector>

#include "util.h"

using namespace std;

typedef long long ll;

ll part1(const string& inp) {
    vector<smatch> matches = util::find_all(inp, regex("mul\\(([0-9]+),([0-9]+)\\)"));
    ll result = 0;
    for (auto& m: matches) {
        result += stoll(m[1]) * stoll(m[2]);    
    }
    return result;
}

ll part2(const string& inp) {
    vector<smatch> matches = util::find_all(inp, regex("do\\(\\)(.+?)don't\\(\\)"));
    ll result = 0;
    for (auto& m: matches) {
        result += part1(m[1]);
    }
    return result;
}

int main(int argc, char** argv) {
    string fname = argv[1];
    ifstream in(fname);
    string line;
    string content = util::join(util::readlines(in), "");
    content = "do()" + content + "don't()";

    cout << part1(content) << "\n";
    cout << part2(content) << "\n";

    return 0;
}
