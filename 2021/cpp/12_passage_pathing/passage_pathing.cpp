#include <iostream>
#include <vector>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

using Edges = std::unordered_map<std::string, std::vector<std::string>>;
using Path = std::vector<std::string>;

bool isSmall(const std::string& s) {
    return std::all_of(s.begin(), s.end(), [](char c) {
        return std::islower(c);
    });
}

int countPaths(Edges& edges, Path& currentPath)
{
    if (currentPath.back() == "end") return 1;
    int result = 0;
    for (auto& next: edges[currentPath.back()])
    {
        if (isSmall(next) and std::find(currentPath.begin(), currentPath.end(), next) != currentPath.end())
            continue;
        currentPath.push_back(next);
        result += countPaths(edges, currentPath);
        currentPath.pop_back();
    }
    return result;
}

int countPaths2(Edges& edges, Path& currentPath, bool visitedTwice)
{
    if (currentPath.back() == "end")
    {
        return 1;
    }
    int result = 0;
    for (auto& next: edges[currentPath.back()])
    {
        bool _visitedTwice = visitedTwice;
        if (next == "start") continue;
        if (isSmall(next) and std::find(currentPath.begin(), currentPath.end(), next) != currentPath.end())
        {
            if (visitedTwice)
                continue;
            visitedTwice = true; 
        }
        currentPath.push_back(next);
        result += countPaths2(edges, currentPath, visitedTwice);
        currentPath.pop_back();
        visitedTwice = _visitedTwice;
    }
    return result;
}

int main()
{
    std::string token;
    Edges paths;
    while (std::cin >> token)
    {
        std::stringstream ss(token);
        std::string start, end;
        getline(ss, start, '-');
        getline(ss, end);
        if (paths.find(start) == paths.end()) paths[start] = {end};
        else paths[start].push_back(end);
        if (paths.find(end) == paths.end()) paths[end] = {start};
        else paths[end].push_back(start);
    }

    Path currentPath = {"start"};
    std::cout << countPaths(paths, currentPath) << "\n";
    std::cout << countPaths2(paths, currentPath, false) << "\n";

    return 0;
}
