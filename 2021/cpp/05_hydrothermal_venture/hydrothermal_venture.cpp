#include <iostream>
#include <sstream>
#include <algorithm>
#include <vector>

struct Coords
{
    int sx, sy, ex, ey;
};

Coords Parse(const std::string& entry)
{
    Coords c;
    std::stringstream ss(entry);
    std::string token;
    getline(ss, token, ',');
    c.sx = std::stoi(token);
    getline(ss, token, ' ');
    c.sy = std::stoi(token);
    getline(ss, token, ' ');
    getline(ss, token, ',');
    c.ex = std::stoi(token);
    getline(ss, token);
    c.ey = std::stoi(token);
    return c;
}

int main()
{
    std::string token;
    std::vector<Coords> cs;
    while (getline(std::cin, token))
        cs.push_back(Parse(token)); 

    std::vector<std::vector<int>> counts(1000);
    for (auto& v: counts)
    {
        for (int i = 0; i < 1000; ++i) v.push_back(0);
    }

    // Part 1
    int overlaps = 0;
    for (const auto& c: cs)
    {
        if (c.sx != c.ex and c.sy != c.ey) continue;

        if (c.sx == c.ex)
        {
            for (int y = std::min(c.sy, c.ey); y <= std::max(c.sy, c.ey); ++y)
            {
                counts[y][c.sx]++;
                if (counts[y][c.sx] == 2) overlaps++;
            }
        }
        if (c.sy == c.ey)
        {
            for (int x = std::min(c.sx, c.ex); x <= std::max(c.sx, c.ex); ++x)
            {
                counts[c.sy][x]++;
                if (counts[c.sy][x] == 2) overlaps++;
            }
        }
    }

    std::cout << overlaps << "\n";

    // Part 2
    for (auto& v: counts)
    {
        for (int i = 0; i < 1000; ++i) v[i] = 0;
    }

    overlaps = 0;
    for (const auto& c: cs)
    {
        if (c.sx != c.ex and c.sy != c.ey)
        {
            for (int i = 0; i <= std::abs(c.sx - c.ex); ++i)
            {
                if (c.sx < c.ex and c.sy < c.ey)
                {
                    if (++counts[c.sy + i][c.sx + i] == 2) overlaps++;
                }
                if (c.sx < c.ex and c.sy > c.ey)
                {
                    if (++counts[c.sy - i][c.sx + i] == 2) overlaps++;
                }
                if (c.sx > c.ex and c.sy < c.ey)
                {
                    if (++counts[c.sy + i][c.sx - i] == 2) overlaps++;
                }
                if (c.sx > c.ex and c.sy > c.ey)
                {
                    if (++counts[c.sy - i][c.sx - i] == 2) overlaps++;
                }
            }
        }
        if (c.sx == c.ex)
        {
            for (int y = std::min(c.sy, c.ey); y <= std::max(c.sy, c.ey); ++y)
            {
                if (++counts[y][c.sx] == 2) overlaps++;
            }
        }
        if (c.sy == c.ey)
        {
            for (int x = std::min(c.sx, c.ex); x <= std::max(c.sx, c.ex); ++x)
            {
                if (++counts[c.sy][x] == 2) overlaps++;
            }
        }
    }

    std::cout << overlaps << "\n";
    return 0;
}
