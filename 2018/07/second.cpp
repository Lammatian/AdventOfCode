#include <iostream>
#include <vector>
#include <fstream>
#include <cmath>
#include <string>
#include <set>
#include <stack>
#include <map>
#include <algorithm>
#include <queue>
#include <functional>

struct Node {
    std::set<char> pre;
    std::set<char> next;
};

char findMinLeft(const std::vector<int>& left) {
    int _min = 100;
    char which = ' ';

    for (int i = 0; i < left.size(); ++i) {
        if (left[i] > 0 && left[i] < _min) {
            _min = left[i];
            which = i + 'A';
        }
    }

    return which;
}

void updateAll(std::vector<int>& left, int value) {
    for (auto &x: left) {
        if (x > 0) {
            x -= value;
        }
    }
}

int main() {
    std::priority_queue<char, std::vector<char>, std::greater<char>> p;
    std::fstream f("input.txt");
    std::string token;
    std::map<char, Node> m;
    std::set<char> start;
    std::vector<int> left(26);

    while (getline(f, token)) {
        char a = token[5];
        char b = token[36];

        m[a].next.insert(b);
        m[b].pre.insert(a);

        if (m[a].pre.size() == 0) {
            start.insert(a);
        }

        if (start.find(b) != start.end()) {
            start.erase(b);
        }
    }

    for (auto &x: start) {
        left[x - 'A'] = 60 + (x - 'A' + 1);
    }

    std::string order;
    int result = 0;
    char top = 'Z';

    while (true) {
        top = findMinLeft(left);

        if (top == ' ') {
            break;
        }

        std::cout << "Doing " << top << " for " << left[top - 'A'] << std::endl;
        result += left[top - 'A'];
        updateAll(left, left[top - 'A']);

        for (auto &x: m[top].next) {
            m[x].pre.erase(top);

            if (m[x].pre.size() == 0) {
                left[x - 'A'] = 60 + (x - 'A' + 1);
            }
        }
    }

    std::cout << result << std::endl;

    return 0;
}