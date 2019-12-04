#include <iostream>
#include <algorithm>
#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>
#include <sstream>
#include <streambuf>
#include <cmath>

int main() {
    std::fstream f("input.txt");
    std::string s((std::istreambuf_iterator<char>(f)),
                  std::istreambuf_iterator<char>());
    f.close();

    int i = 0;
    int gap = abs('A' - 'a');

    while (i < s.size() - 1) {
        if (abs(s[i] - s[i + 1]) == gap) {
            s = s.substr(0, i) + s.substr(i + 2);
            i = std::max(0, i - 1);
        } else {
            i++;
        }
    }

    std::cout << s.length() << std::endl;

    return 0;
}