#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

using namespace std;
using ll = long long;

struct Point
{
    int r, c;
};

int main()
{
    vector<vector<int>> points;
    string token;
    while (getline(cin, token))
    {
        char c;
        vector<int> row;
        stringstream ss(token);
        while (ss >> c)
            row.push_back(c - '0');
        points.push_back(row);
    }

    // Part 1
    ll result = 0;
    for (int r = 0; r < points.size(); ++r)
    {
        for (int c = 0; c < points[r].size(); ++c)
        {
            if (r > 0 and points[r - 1][c] <= points[r][c])
                continue;
            if (r < points.size() - 1 and points[r + 1][c] <= points[r][c])
                continue;
            if (c > 0 and points[r][c - 1] <= points[r][c])
                continue;
            if (c < points[r].size() - 1 and points[r][c + 1] <= points[r][c])
                continue;

            result += (points[r][c] + 1);
        }
    }

    cout << result << "\n";

    // Part 2
    vector<Point> centres;
    for (int r = 0; r < points.size(); ++r)
    {
        for (int c = 0; c < points[r].size(); ++c)
        {
            if (r > 0 and points[r - 1][c] <= points[r][c])
                continue;
            if (r < points.size() - 1 and points[r + 1][c] <= points[r][c])
                continue;
            if (c > 0 and points[r][c - 1] <= points[r][c])
                continue;
            if (c < points[r].size() - 1 and points[r][c + 1] <= points[r][c])
                continue;

            centres.push_back(Point{r, c});
        }
    }

    int m1 = 0;
    int m2 = 0;
    int m3 = 0;
    for (auto& p: centres)
    {
        int basinSize = 0;
        points[p.r][p.c] = 9;
        vector<Point> queue = {p};
        while (not queue.empty())
        {
            Point current = queue.back();
            queue.pop_back();
            basinSize++;
            if (current.r > 0 and points[current.r - 1][current.c] < 9)
            {
                points[current.r - 1][current.c] = 9;
                queue.push_back({current.r - 1, current.c});
            }
            if (current.r < points.size() - 1 and points[current.r + 1][current.c] < 9)
            {
                points[current.r + 1][current.c] = 9;
                queue.push_back({current.r + 1, current.c});
            }
            if (current.c > 0 and points[current.r][current.c - 1] < 9)
            {
                points[current.r][current.c - 1] = 9;
                queue.push_back({current.r, current.c - 1});
            }
            if (current.c < points[current.r].size() - 1 and points[current.r][current.c + 1] < 9)
            {
                points[current.r][current.c + 1] = 9;
                queue.push_back({current.r, current.c + 1});
            }
        }

        if (basinSize > m1)
        {
            m3 = m2;
            m2 = m1;
            m1 = basinSize;
        }
        else if (basinSize > m2)
        {
            m3 = m2;
            m2 = basinSize;
        }
        else if (basinSize > m3)
        {
            m3 = basinSize;
        }
    }

    std::cout << m1 * m2 * m3 << "\n";

    return 0;
}
