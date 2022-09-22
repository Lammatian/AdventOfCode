#include <iostream>
#include <vector>
#include <algorithm>

using ll = long long;

int main()
{
    std::vector<ll> positions;
    std::string token;
    ll min = 1e9;
    ll max = 0;
    while (getline(std::cin, token, ','))
    {
        ll num = std::stoll(token);
        min = std::min(min, num);
        max = std::max(max, num);
        positions.push_back(std::stoll(token));
    }

    // Part 1
    ll minFuel = 1e9;
    for (int i = min; i <= max; ++i)
    {
        ll fuel = 0;
        for (auto p: positions)
            fuel += std::abs(i - p);

        minFuel = std::min(minFuel, fuel);
    }

    std::cout << minFuel << "\n";

    // Part 2
    minFuel = 1e10;
    for (int i = min; i <= max; ++i)
    {
        ll fuel = 0;
        for (auto p: positions)
        {
            fuel += std::abs(i - p) * (std::abs(i - p) + 1) / 2;
        }

        minFuel = std::min(minFuel, fuel);
    }

    std::cout << minFuel << "\n";

    return 0;
}
