#include <iostream>
#include <vector>

int INPUT = 768071;
//int INPUT = 9;

int main() {
    std::vector<int> recipes = {3, 7};
    int c1 = 0;
    int c2 = 1;

    while (recipes.size() < INPUT + 12) {
        int next = recipes[c1] + recipes[c2];

        if (next < 10) {
            recipes.push_back(next);
        } else {
            recipes.push_back(next/10);
            recipes.push_back(next%10);
        }

        c1 = (c1 + recipes[c1] + 1) % recipes.size();
        c2 = (c2 + recipes[c2] + 1) % recipes.size();
    }

    for (int i = INPUT; i < INPUT + 10; ++i) {
        std::cout << recipes[i];
    }

    std::cout << std::endl;

    return 0;
}