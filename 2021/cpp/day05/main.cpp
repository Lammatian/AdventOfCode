#include "../utils.h"

struct P
{
    ll sx, sy, ex, ey;
};

int main()
{
    string token;
    vector<P> ps;
    while (getline(cin, token))
    {
        stringstream ss(token);
        string val;
        P p;
        getline(ss, val, ',');
        p.sx = stoll(val);
        getline(ss, val, ' ');
        p.sy = stoll(val);
        getline(ss, val, ' ');
        getline(ss, val, ',');
        p.ex = stoll(val);
        getline(ss, val);
        p.ey = stoll(val);
        ps.push_back(p);
    }

    // Part 1
    vector<vector<ll>> grid = vec2d(1000, 0LL);
    ll result = 0;
    for (auto& p: ps)
    {
        if (p.sx == p.ex)
        {
            for (int y = min(p.sy, p.ey); y <= max(p.sy, p.ey); ++y)
            {
                grid[p.sx][y]++;
                if (grid[p.sx][y] == 2) result++;
            }
        }
        if (p.sy == p.ey)
        {
            for (int x = min(p.sx, p.ex); x <= max(p.sx, p.ex); ++x)
            {
                grid[x][p.sy]++;
                if (grid[x][p.sy] == 2) result++;
            }
        }
    }
    print(result);
    for (int i = 0; i < 1000; ++i)
    {
        grid[i] = vector<ll>(1000, 0);
    }

    // Part 2
    result = 0;
    grid = vec2d(1000, 0LL);

    for (auto& p: ps)
    {
        if (p.sx == p.ex)
        {
            for (int y = min(p.sy, p.ey); y <= max(p.sy, p.ey); ++y)
            {
                grid[p.sx][y]++;
                if (grid[p.sx][y] == 2) result++;
            }
        }
        else if (p.sy == p.ey)
        {
            for (int x = min(p.sx, p.ex); x <= max(p.sx, p.ex); ++x)
            {
                grid[x][p.sy]++;
                if (grid[x][p.sy] == 2) result++;
            }
        }
        else
        {
            for (int i = 0; i <= abs(p.sx - p.ex); ++i)
            {
                if (p.sx < p.ex and p.sy < p.ey)
                {
                    grid[p.sx + i][p.sy + i]++;
                    if (grid[p.sx + i][p.sy + i] == 2) result++;
                }
                if (p.sx < p.ex and p.sy > p.ey)
                {
                    grid[p.sx + i][p.sy - i]++;
                    if (grid[p.sx + i][p.sy - i] == 2) result++;
                }
                if (p.sx > p.ex and p.sy < p.ey)
                {
                    grid[p.sx - i][p.sy + i]++;
                    if (grid[p.sx - i][p.sy + i] == 2) result++;
                }
                if (p.sx > p.ex and p.sy > p.ey)
                {
                    grid[p.sx - i][p.sy - i]++;
                    if (grid[p.sx - i][p.sy - i] == 2) result++;
                }
            }
        }
    }

    print(result);

    return 0;
}
