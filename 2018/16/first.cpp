#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
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

    int countAmbiguous = 0;

    while (getline(f, before)) {
        regs = getRegs(before); 
        getline(f, command);
        op = getOp(command);
        int a = op[1];
        int b = op[2];
        int c = op[3];
        getline(f, after); 
        regs2 = getRegs(after);

        int count = 0;

        for (auto &f: ifn) {
            if (regs2[c] == f(regs[a], regs[b])) {
                count++;
            }

            if (regs2[c] == f(regs[a], b)) {
                count++;
            }
        }

        for (auto &f: bfn) {
            if (regs2[c] == (f(regs[a], regs[b]) ? 1 : 0)) {
                count++;
            }

            if (regs2[c] == (f(regs[a], b) ? 1 : 0)) {
                count++;
            }

            if (regs2[c] == (f(a, regs[b]) ? 1 : 0)) {
                count++;
            }
        }
        
        // set function
        if (regs2[c] == regs[a]) {
            count++;
        }

        if (regs2[c] == a) {
            count++;
        }

        if (count >= 3) {
            countAmbiguous++;
        }

        getline(f, before);
    }

    std::cout << countAmbiguous << std::endl;

    return 0;
}