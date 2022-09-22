#include "../utils.h"

int main()
{
    vector<ll> ps = readInts(cin, ',');
    printCont(ps);

    ll _min = 1e9;
    ll _max = 0;
    for (auto p: ps)
    {
        _min = min(_min, p);
        _max = max(_max, p);
    }

    // Part 1
    ll result = 1e9;
    for (int i = _min; i <= _max; ++i)
    {
        ll cost = 0;
        for (auto& p: ps) cost += abs(i - p); 
        result = min(result, cost);
    }

    print(result);

    // Part 2
    result = 1e9;
    for (int i = _min; i <= _max; ++i)
    {
        ll cost = 0;
        for (auto& p: ps) cost += abs(i - p) * (abs(i - p) + 1) / 2; 
        result = min(result, cost);
    }
    print(result);

    return 0;
}
