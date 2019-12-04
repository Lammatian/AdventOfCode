#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <functional>

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

struct Function {
    std::function<int(int, int)> fn;
    int type;
    /*
    0 - r/rr
    1 - i/ri
    2 - ir
    3 - setr
    4 - seti
    */
};

int main() {
    std::fstream f("input2.txt");
    std::string command;
    std::vector<int> regs(4, 0);

    std::vector<Function> fns {
        {std::plus<int>(), 1},
        {std::bit_and<int>(), 1},
        {std::greater<int>(), 2},
        {std::bit_or<int>(), 0},
        {std::equal_to<int>(), 0},
        {std::bit_or<int>(), 1},
        {std::greater<int>(), 0},
        {std::plus<int>(), 3},
        {std::multiplies<int>(), 1},
        {std::plus<int>(), 4},
        {std::bit_and<int>(), 0},
        {std::greater<int>(), 1},
        {std::equal_to<int>(), 2},
        {std::equal_to<int>(), 1},
        {std::plus<int>(), 0},
        {std::multiplies<int>(), 0}
    };

    int countAmbiguous = 0;

    while (getline(f, command)) {
        std::vector<int> op = getOp(command);
        int code = op[0];
        int a = op[1];
        int b = op[2];
        int c = op[3];

        switch (fns[code].type) {
            case 0:
                regs[c] = fns[code].fn(regs[a], regs[b]);
                break;
            case 1:
                regs[c] = fns[code].fn(regs[a], b);
                break;
            case 2:
                regs[c] = fns[code].fn(a, regs[b]);
                break;
            case 3:
                regs[c] = fns[code].fn(regs[a], 0);
                break;
            case 4:
                regs[c] = fns[code].fn(a, 0);
                break;
        }

        for (auto &r: regs) {
            std::cout << r << " ";
        }

        std::cout << std::endl;
    }

    return 0;
}