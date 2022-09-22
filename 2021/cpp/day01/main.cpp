#include "../utils.h"

int main()
{
    ll inc = 0;
    ll last = 1e9;
    ll curr = 0;
    vector<ll> measurements = readInts(cin, '\n');

    // Part 1
    for (auto m: measurements)
    {
        if (m > last) inc++;
        last = m;
    }
    print(inc);

    // Part 2
    inc = 0;
    for (int i = 0; i + 3 < measurements.size(); ++i)
    {
        if (measurements[i + 3] > measurements[i]) inc++;
    }
    print(inc);

    return 0;
}
