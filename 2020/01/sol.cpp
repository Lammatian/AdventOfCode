#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include "util.h"

// Find product of two numbers summing to 2020
int sol1(std::vector<ll> nums) {
    std::set<ll> seen;

    for (auto& n: nums) {
        if (seen.find(n) != seen.end()) {
            return n * (2020 - n);
        }

        seen.insert(2020 - n);
    }

    return -1;
}

struct pair_sum {
    ll sum;
    ll n1;
    ll n2;

    // Doesn't need to seem compare, but just for completeness 
    bool operator<(const pair_sum& ps) const
    {
        return sum < ps.sum;
    }
};

// Find product of three numbers summing to 2020
int sol2(std::vector<ll> nums) {
    std::map<ll, std::pair<ll, ll>> seen_pairs;

    for (int i = 0; i < nums.size(); ++i) {
        for (int j = i + 1; j < nums.size(); ++j) {
            seen_pairs[2020 - nums[i] - nums[j]] = {nums[i], nums[j]};
        }
    }

    for (auto& n: nums) {
        if (seen_pairs.find(n) != seen_pairs.end()) {
            return n * seen_pairs[n].first * seen_pairs[n].second;
        }
    }

    return -1;

    // alternative approach: sort -> two pointers, also O(n^2)
}

int main() {
    std::ifstream f("input.txt");
    std::vector<ll> nums = util::readlls(f);

    std::cout << sol1(nums) << std::endl;
    std::cout << sol2(nums) << std::endl;

    return 0;
}