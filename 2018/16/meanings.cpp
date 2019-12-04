#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <functional>

std::vector<int> getRegs(std::string s) {
    std::vector<int> result;
    std::stringstream ss(s);
    std::string token;

    getline(ss, token, '[');
    getline(ss, token, ',');
    result.push_back(std::stoi(token));
    getline(ss, token, ',');
    result.push_back(std::stoi(token));
    getline(ss, token, ',');
    result.push_back(std::stoi(token));
    getline(ss, token, ']');
    result.push_back(std::stoi(token));
    
    return result;
}

std::vector<int> getOp(std::string s) {
    std::vector<int> result;
    std::stringstream ss(s);
    std::string token;

    getline(ss, token, ' ');
    result.push_back(std::stoi(token));
    getline(ss, token, ' ');
    result.push_back(std::stoi(token));
    getline(ss, token, ' ');
    result.push_back(std::stoi(token));
    getline(ss, token);
    result.push_back(std::stoi(token));
    
    return result;
}

int main() {
    std::fstream f("input1.txt");
    std::string before;
    std::string command;
    std::string after;
    std::vector<int> regs;
    std::vector<int> op;
    std::vector<int> regs2;

    std::vector<std::function<int(int, int)>> ifn = {
        std::plus<int>(),
        std::multiplies<int>(),
        std::bit_and<int>(),
        std::bit_or<int>(),
    };

    std::vector<std::function<int(int, int)>> bfn = {
        std::greater<int>(),
        std::equal_to<int>()
    };

    std::vector<std::string> f2s = {
        "plus",
        "mult",
        "band",
        "bor",
        "gt",
        "eq"
    };

    std::vector<std::set<std::string>> possibleMeanings(16);
    std::vector<bool> canBeSetr(16, true);
    std::vector<bool> canBeSeti(16, true);

    for (int i = 0; i < 16; ++i) {
        for (int j = 0; j < 4; ++j) {
            possibleMeanings[i].insert(f2s[j] + "r"); // 0 - r
            possibleMeanings[i].insert(f2s[j] + "i"); // 1 - i
        }

        for (int j = 0; j < 2; ++j) {
            possibleMeanings[i].insert(f2s[4 + j] + "ri"); // 0 - ri
            possibleMeanings[i].insert(f2s[4 + j] + "ir"); // 1 - ir
            possibleMeanings[i].insert(f2s[4 + j] + "rr"); // 2 - rr
        }
    }

    while (getline(f, before)) {
        regs = getRegs(before); 
        getline(f, command);
        op = getOp(command);
        int code = op[0];
        int a = op[1];
        int b = op[2];
        int c = op[3];
        getline(f, after); 
        regs2 = getRegs(after);

        int count = 0;

        for (int i = 0; i < 4; ++i) {
            auto f = ifn[i];

            if (regs2[c] == f(regs[a], regs[b])) {
                count++;
            } else {
                possibleMeanings[code].erase(f2s[i] + "r");
            }

            if (regs2[c] == f(regs[a], b)) {
                count++;
            } else {
                possibleMeanings[code].erase(f2s[i] + "i");
            }
        }

        for (int i = 0; i < 2; ++i) {
            auto f = bfn[i];

            if (regs2[c] == (f(regs[a], regs[b]) ? 1 : 0)) {
                count++;
            } else {
                possibleMeanings[code].erase(f2s[4 + i] + "rr");
            }

            if (regs2[c] == (f(regs[a], b) ? 1 : 0)) {
                count++;
            } else {
                possibleMeanings[code].erase(f2s[4 + i] + "ri");
            }

            if (regs2[c] == (f(a, regs[b]) ? 1 : 0)) {
                count++;
            } else {
                possibleMeanings[code].erase(f2s[4 + i] + "ir");
            }
        }
        
        // set function
        if (regs2[c] == regs[a]) {
            count++;
        } else {
            canBeSetr[code] = false;
        }

        if (regs2[c] == a) {
            count++;
        } else {
            canBeSeti[code] = false;
        }

        getline(f, before);
    }

    std::vector<std::set<std::string>> meanings(16);

    for (int i = 0; i < 16; ++i) {
        for (auto &s: possibleMeanings[i]) {
            meanings[i].insert(s);
        }

        if (canBeSeti[i]) {
            meanings[i].insert("seti");
        }

        if (canBeSetr[i]) {
            meanings[i].insert("setr");
        }
    }

    // Find an element with 1
    std::string toRemove;
    for (int i = 0; i < meanings.size(); ++i) {
        if (meanings[i].size() == 1) {
            toRemove = *meanings[i].begin();
            std::cout << i << ": " << toRemove << std::endl;
            meanings[i].clear();

            for (auto &s: meanings) {
                s.erase(toRemove);
            }

            i = -1;
        }
    }

    return 0;
}