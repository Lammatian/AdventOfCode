#include "../utils.h"

int main()
{
    vector<ll> vals = readInts(cin, ',');
    unordered_map<ll, ll> counts;
    for (auto& v: vals)
        counts[v]++;

    // Part 1
    for (int i = 0; i < 80; ++i)
    {
        ll newFish = counts[0];
        for (int j = 0; j <= 7; j++)
        {
            counts[j] = counts[j + 1];
        }
        counts[8] = newFish;
        counts[6] += newFish;
    }
    ll result = 0;
    for (auto& [k, v]: counts)
        result += v;
    print(result);

    // Part 2
    for (int i = 0; i < 256 - 80; ++i)
    {
        ll newFish = counts[0];
        for (int j = 0; j <= 7; j++)
        {
            counts[j] = counts[j + 1];
        }
        counts[8] = newFish;
        counts[6] += newFish;
    }
    result = 0;
    for (auto& [k, v]: counts)
        result += v;
    print(result);
    return 0;
}
