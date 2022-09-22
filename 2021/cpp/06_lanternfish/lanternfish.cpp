#include <iostream>
#include <unordered_map>
#include <vector>

void UpdateCounts(std::unordered_map<long long, long long>& counts)
{
    long long newFish = counts[0];
    for (int i = 1; i <= 8; ++i)
    {
        counts[i - 1] = counts[i];
    }
    counts[6] += newFish;
    counts[8] = newFish;
}

int main()
{
    std::unordered_map<long long, long long> init;
    std::string token;
    while (getline(std::cin, token, ','))
        init[std::stoi(token)]++;

    // Part 1
    for (int i = 0; i < 80; ++i)
    {
        UpdateCounts(init);
    }

    long long result = 0;
    for (auto [k, v]: init)
        result += v;

    std::cout << result << "\n";

    // Part 2
    for (int i = 0; i < 256 - 80; ++i)
    {
        UpdateCounts(init);
    }

    result = 0;
    for (auto [k, v]: init)
        result += v;

    std::cout << result << "\n";

    return 0;
}
