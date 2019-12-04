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

    int min = s.length();

    for (int i = 0; i < 26; ++i) {
        std::string temp;
        char curr = 'a' + i;

        for (auto &c: s) {
            if (c != curr && abs(c - curr) != gap) {
                temp += c;
            }
        }

        int j = 0;
        while (j < temp.size() - 1) {
            if (abs(temp[j] - temp[j + 1]) == gap) {
                temp = temp.substr(0, j) + temp.substr(j + 2);
                j = std::max(0, j - 1);
            } else {
                j++;
            }
        }

        min = std::min(min, (int)temp.length());
    }

    std::cout << min << std::endl;

    return 0;
}
