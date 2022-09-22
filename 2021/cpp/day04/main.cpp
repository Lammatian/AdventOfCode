#include "../utils.h"

int main()
{
    vector<ll> nums = readIntLine(cin, ',');
    vector<vector<ll>> bingos;
    vector<ll> bingo;
    string token;
    while (getline(cin, token))
    {
        vector<ll> bingo;
        for (int i = 0; i < 5; ++i)
        {
            vector<ll> row = readIntLine(cin);
            bingo.insert(bingo.end(), row.begin(), row.end());
        }
        bingos.push_back(bingo);
    }

    auto rowDone = [](const vector<ll>& bingo, int r)
    {
        for (int i = 5 * r; i < 5 * r + 5; ++i)
        {
            if (bingo[i]) return false;
        }

        return true;
    };
    auto colDone = [](const vector<ll>& bingo, int c)
    {
        for (int i = c; i <= 25; i += 5)
        {
            if (bingo[i]) return false;
        }

        return true;
    };
    auto anyRowDone = [&](const vector<ll>& bingo)
    {
        for (int i = 0; i < 5; ++i)
        {
            if (rowDone(bingo, i)) return true;
        }

        return false;
    };
    auto anyColDone = [&](const vector<ll>& bingo)
    {
        for (int i = 0; i < 5; ++i)
        {
            if (colDone(bingo, i)) return true;
        }

        return false;
    };
    auto isBingo = [&](const vector<ll>& bingo)
    {
        return anyRowDone(bingo) or anyColDone(bingo);
    };
    auto remove = [&](vector<ll>& bingo, int b)
    {
        for (auto& v: bingo)
        {
            if (v == b) v = 0;
        }
    };
    auto sum = [&](vector<ll>& bingo)
    {
        return accumulate(bingo.begin(), bingo.end(), 0);
    };

    // Part 1
    bool done = false;
    vector<vector<ll>> theBingos = bingos;
    for (auto& n: nums)
    {
        for (auto& b: theBingos)
        {
            remove(b, n);
            if (isBingo(b))
            {
                print(n * sum(b));
                done = true;
                break;
            }
        }
        if (done) break;
    }

    // Part 2
    int finished = 0;
    done = false;
    theBingos = bingos;
    for (auto& n: nums)
    {
        for (auto& b: theBingos)
        {
            if (isBingo(b)) continue;
            remove(b, n);
            if (isBingo(b)) finished++;
            if (finished == bingos.size())
            {
                print(n * sum(b));
                done = true;
                break;
            }
        }
        if (done) break;
    }

    return 0;
}
