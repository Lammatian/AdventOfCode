#include <iostream>
#include <vector>

int INPUT = 768071;
//int INPUT = 01245;
//int INPUT = 92510;
//int INPUT = 59414;

bool areInput(const std::vector<int>& r, int s) {
    int i = r.size() - s;
    int x = INPUT;

    while (x > 0 && i >= 0) {
        if (r[i] != x%10) {
            return false;
        }

        x /= 10;
        i--;
    }

    return i >= 0;
}

int main() {
    std::vector<int> recipes = {3, 7};
    long long c1 = 0;
    long long c2 = 1;

    while (!areInput(recipes, 1) && !areInput(recipes, 2)) {
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

    std::cout << recipes.size() - std::to_string(INPUT).length() - (areInput(recipes, 2) ? 1 : 0) << std::endl;

    return 0;
}
