#include <iostream>
#include <fstream>
#include <algorithm>
#include <map>

#include "util.h"

ll sol1(const std::vector<ll>& nums, int window) {
    std::map<ll, int> available;

    for (int i = 0; i < window; ++i) {
        for (int j = i + 1; j < window; ++j) {
            if (nums[i] != nums[j]) {
                available[nums[i] + nums[j]]++;
            }
        }
    }

    for (int i = window; i < nums.size(); ++i) {
        if (available[nums[i]] == 0) {
            return nums[i];
        }

        for (int j = 1; j < window; ++j) {
            available[nums[i - window] + nums[i - window + j]]--;

            if (available[nums[i - window] + nums[i - window + j]] == 0) {
                available.erase(nums[i - window] + nums[i - window + j]);
            }

            available[nums[i] + nums[i - window + j]]++;
        }
    }

    return -1;
}

/**
 * I am actually not sure if this works 100% of the time,
 * but seems to do the trick here so I'll stick with it
 **/
ll sol2(const std::vector<ll>& nums, ll target) {
    ll start_idx = 0;
    ll end_idx = 0;
    ll cur_sum = nums[0];

    while (end_idx < nums.size()) {
        if (cur_sum < target) {
            end_idx++;
            cur_sum += nums[end_idx];
        } else if (cur_sum > target) {
            cur_sum -= nums[start_idx];
            start_idx++;
        } else {
            auto minmax = std::minmax_element(&nums[start_idx], &nums[end_idx]);
            return *minmax.first + *minmax.second;
        }
    }

    return -1;
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    int window = argc > 2 ? std::stoi(argv[2]) : 25;
    std::ifstream f(filename);

    std::vector<std::string> lines = util::readlines(f);
    std::vector<ll> nums(lines.size());
    std::transform(lines.begin(), lines.end(), nums.begin(),
                   [](std::string s){return std::stoll(s);});

    ll solution_1 = sol1(nums, window);
    std::cout << solution_1 << std::endl;
    std::cout << sol2(nums, solution_1) << std::endl;

    return 0;
}