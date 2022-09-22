#include "../utils.h"

int main()
{
    vector<string> vals;
    string token;
    while (cin >> token)
    {
        vals.push_back(token);
    }

    // Part 1
    unordered_map<ll, ll> counts;
    for (auto& v: vals)
    {
        for (int i = 0; i < v.size(); ++i)
        {
            counts[i] += (v[i] == '1');
        }
    }

    string gamma;
    string epsilon;
    for (int i = 0; i < vals[0].size(); ++i)
    {
        if (counts[i] > vals.size() / 2)
        {
            gamma += "1";
            epsilon += "0";
        }
        else
        {
            gamma += "0";
            epsilon += "1";
        }
    }

    print(stoi(gamma, nullptr, 2) * stoi(epsilon, nullptr, 2));

    // Part 2
    ll position = 0;
    vector<string> vals1 = vals;
    while (vals1.size() != 1)
    {
        ll count = 0;
        for (auto& v: vals1)
        {
            count += v[position] == '1';
        }

        char filter;
        if (2 * count >= vals1.size()) filter = '1';
        else filter = '0';

        vector<string> tmp;
        for (auto v: vals1)
        {
            if (v[position] == filter) tmp.push_back(v);
        }
        vals1 = tmp;
        position++;
    }

    position = 0;
    vector<string> vals2 = vals;
    while (vals2.size() != 1)
    {
        ll count = 0;
        for (auto& v: vals2)
        {
            count += v[position] == '1';
        }

        char filter;
        if (2 * count < vals2.size()) filter = '1';
        else filter = '0';

        vector<string> tmp;
        for (auto v: vals2)
        {
            if (v[position] == filter) tmp.push_back(v);
        }
        vals2 = tmp;
        position++;
    }
    print(stoi(vals1[0], nullptr, 2) * stoi(vals2[0], nullptr, 2));

    return 0;
}
