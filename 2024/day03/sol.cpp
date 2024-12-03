#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <vector>
using namespace std;

typedef long long ll;

ll part1(const string& inp) {
    smatch mr;
    const regex r = regex("mul\\(([0-9]+),([0-9]+)\\)");
    string::const_iterator searchStart(inp.cbegin());
    ll result = 0;
    while (regex_search(searchStart, inp.cend(), mr, r)) {
        result += stoll(mr[1]) * stoll(mr[2]);
        searchStart = mr.suffix().first;
    }

    return result;
}

ll part2(const string& inp) {
    smatch mr;
    const regex r = regex("do\\(\\)(.+?)don't\\(\\)");
    string::const_iterator searchStart(inp.cbegin());
    ll result = 0;
    while (regex_search(searchStart, inp.cend(), mr, r)) {
        result += part1(mr[1]);
        searchStart = mr.suffix().first;
    }
    return result;
}

int main(int argc, char** argv) {
    string fname = argv[1];
    ifstream in(fname);
    string line;
    string content;
    ll result = 0;
    while (getline(in, line)) {
        content += line;
    }
    content = "do()" + content + "don't()";

    cout << part1(content) << "\n";
    cout << part2(content) << "\n";

    return 0;
}
