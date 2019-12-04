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

int main() {
    std::priority_queue<char, std::vector<char>, std::greater<char>> p;
    std::fstream f("input.txt");
    std::string token;
    std::map<char, Node> m;
    std::set<char> start;

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
        p.push(x);
    }

    std::string order;

    while (!p.empty()) {
        char top = p.top();
        p.pop();
        order += top;

        for (auto &x: m[top].next) {
            m[x].pre.erase(top);

            if (m[x].pre.size() == 0) {
                p.push(x);
            }
        }
    }

    std::cout << order << std::endl;

    return 0;
}