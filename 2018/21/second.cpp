#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>

typedef long long ll;

int main() {
    std::vector<ll> nums;
    ll r0 = 0;
    ll r1 = 0;
    ll r2 = 65536;
    ll r3 = 0;
    ll r4 = 6152285;
    ll r5 = 0;

    int count = 0;
    int repeats = 0;

    while (true) {
        r1 = r2 & 255;
        r4 += r1;
        r4 = r4 & 16777215;
        r4 *= 65899;
        r4 = r4 & 16777215;

        if (r2 < 256) {
            std::cout << "In 28-30 with r4: " << r4 << std::endl;
            if (std::find(nums.begin(), nums.end(), r4) != nums.end()) {
                std::cout << "Repeat!" << std::endl;
                repeats++;
                if (repeats == nums.size()) {
                    std::cout << "Last number: " << *nums.rbegin() << std::endl;
                    return 0;
                }
            } else {
                nums.push_back(r4);
                std::cout << "Nums size: " << nums.size() << std::endl;
            }

            if (r0 == r4) {
                std::cout << "Exiting" << std::endl;
            } else {
                std::cout << "Reseting r2, r4" << std::endl;
                r2 = r4 | 65536;
                r4 = 6152285;
                continue;
            }
        }

        r2 = r2/256;
        count++;
    }
    
    return 0;
}