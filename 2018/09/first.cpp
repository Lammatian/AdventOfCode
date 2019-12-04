#include <iostream>
#include <list>
#include <vector>
#include <map>

int main() {
    int p = 400;
    int last = 7186400;
    std::list<int> l;
    std::vector<int> v;
    std::map<int, long long> points;

    l.push_back(0);
    std::list<int>::iterator curr = l.begin();
    int c = 1;
    int pos = 0;
    int lpos = 0;
    long long maxp = 0;

    while (c <= last) {
        if (c % 23 == 0) {
            pos = (pos - 7 + l.size()) % l.size();
            std::advance(curr, pos - lpos);
            lpos = pos;
            points[c % p] += c + *curr;
            maxp = std::max(points[c % p], maxp);
            curr = l.erase(curr);
        } else {
            pos = (pos + 1) % l.size();
            pos++;
            std::advance(curr, pos - lpos);
            lpos = pos;

            if (curr == l.end()) {
                l.push_back(c);
                curr--;
            } else {
                l.insert(curr, c);
                curr--;
            }
        }

        c++;
    }

    std::cout << maxp << std::endl;

    return 0;
}