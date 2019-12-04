#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

struct Count {
    int nodes;
    int meta;
};

int main() {
    std::fstream f("input.txt");
    std::stack<Count> s;

    int a, b;
    f >> a >> b;

    s.push({a, b});
    int result = 0;

    while (!s.empty()) {
        Count c = s.top();

        if (c.nodes == 0 && c.meta == 0) {
            s.pop();
            continue;
        } else if (c.nodes == 0) {
            while (c.meta > 0) {
                int m;
                f >> m;
                result += m;
                c.meta--;
            }
            s.pop();
        } else {
            s.pop();
            s.push({c.nodes - 1, c.meta});
            f >> a >> b;
            s.push({a, b}); 
        }
    }

    std::cout << result << std::endl;

    return 0;
}