#include "../utils.h"

bool IsSmall(const string& c)
{
    return c != "start" and c != "end" and c[0] == tolower(c[0]);
}

ll FindPaths(const string& current, unordered_map<string, vector<string>>& paths, unordered_set<string>& visited)
{
    if (current == "end") return 1;

    ll result = 0;
    for (const auto& n: paths[current])
    {
        if (n == "start") continue;
        if (IsSmall(n) and visited.find(n) != visited.end()) continue;
        visited.insert(n);
        result += FindPaths(n, paths, visited);
        visited.erase(n);
    }
    return result;
}

ll FindPaths2(const string& current, unordered_map<string, vector<string>>& paths, unordered_set<string>& visited, bool visitedTwice)
{
    if (current == "end") return 1;

    ll result = 0;
    for (const auto& n: paths[current])
    {
        bool marked = false;
        if (n == "start")
        {
            continue;
        }
        if (IsSmall(n))
        {
            if (visited.find(n) != visited.end() && visitedTwice)
            {
                continue;
            }
            else if (visited.find(n) != visited.end())
            {
                marked = true;
                visitedTwice = true;
            }
        }
        visited.insert(n);
        result += FindPaths2(n, paths, visited, visitedTwice);
        if (marked) visitedTwice = false;
        else visited.erase(n);
    }
    return result;
}

int main()
{
    unordered_map<string, vector<string>> edges;
    string token;
    while (getline(cin, token, '-'))
    {
        string start = token;
        getline(cin, token);
        string end = token;
        edges[start].push_back(end);
        edges[end].push_back(start);
    }
    unordered_set<string> visited;
    print(FindPaths("start", edges, visited));
    print(FindPaths2("start", edges, visited, false));

    return 0;
}
