#include <iostream>
#include <fstream>
#include <algorithm>
#include <map>
#include <set>

#include "util.h"

ll sol1(std::vector<ll> nums) {
    std::sort(nums.begin(), nums.end());
    std::map<ll, int> diffs;

    ll last = 0;
    std::for_each(nums.begin(), nums.end(),
                  [&diffs, &last](ll num) {
                      diffs[num - last]++;
                      last = num;
                  });

    // device's adapter
    diffs[3]++;

    return diffs[1] * diffs[3];
}

ll sol2(std::vector<ll> nums) {
    ll result = 1;
    // Enhanced numbers - with 0 at the start and max(nums) + 3 at the end
    std::vector<ll> enums;
    enums.push_back(0);
    enums.insert(enums.end(), nums.begin(), nums.end());
    std::sort(enums.begin(), enums.end());
    enums.push_back(enums.back() + 3);

    ll last = -3;
    // Count of the number of numbers between consecutive differences of 3
    std::vector<ll> diff3_counts;
    ll diff3_count = -1;

    for (auto& n: enums) {
        if (n - last == 1) {
            diff3_count++;
        } else if (n - last == 3) {
            diff3_counts.push_back(diff3_count);
            diff3_count = -1;
        } else {
            std::cerr << "That didn't go as expected" << std::endl;
            return -1;
        }

        last = n;
    }

    for (auto& c: diff3_counts) {
        if (c == 1) {
            result *= 2;
        } else if (c == 2) {
            result *= 4;
        } else if (c == 3) {
            result *= 7;
        }
    }

    return result;
}

ll sol2_dp(std::vector<ll> nums) {
    nums.push_back(0);
    std::sort(nums.begin(), nums.end());

    std::map<ll, ll> cache;
    cache[nums.back() + 3] = 1;

    for (auto it = nums.rbegin(); it != nums.rend(); it++) {
        cache[*it] = cache[*it + 1] + cache[*it + 2] + cache[*it + 3];
    }

    return cache[0];
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<ll> nums = util::readlls(f);

    std::cout << sol1(nums) << std::endl;
    std::cout << sol2(nums) << std::endl;
    std::cout << sol2_dp(nums) << std::endl;

    return 0;
}