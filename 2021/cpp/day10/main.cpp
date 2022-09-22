#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <utility>

using namespace std;
using ll = long long;

template<char delim = ' '>
void Print() { cout << "\n"; }

template<char delim = ' ', typename First, typename ...Rest>
void Print(First&& first, Rest&& ...rest)
{
    cout << forward<First>(first) << delim;
    Print<delim>(forward<Rest>(rest)...);
}

int main()
{
    unordered_map<char, ll> points =
    {
        {')', 3},
        {']', 57},
        {'}', 1197},
        {'>', 25137}
    };
    unordered_map<char, char> match =
    {
        {')', '('},
        {']', '['},
        {'}', '{'},
        {'>', '<'}
    };
    vector<char> opening = {'(', '[', '{', '<'};

    vector<string> lines;
    string token;
    while (getline(cin, token))
    {
        lines.push_back(token);
    }

    // Part 1
    ll result = 0;
    for (auto& line: lines)
    {
        vector<char> stack;
        for (auto& c: line)
        {
            if (find(opening.begin(), opening.end(), c) != opening.end())
            {
                stack.push_back(c); 
                continue;
            }
            
            if (stack.back() != match[c])
            {
                result += points[c];
                break;
            }

            stack.pop_back();
        }
    }
    
    Print(result);

    // Part 2
    unordered_map<char, ll> cpoints =
    {
        {')', 1},
        {']', 2},
        {'}', 3},
        {'>', 4}
    };
    unordered_map<char, char> cmatch =
    {
        {'(', ')'},
        {'[', ']'},
        {'{', '}'},
        {'<', '>'}
    };
    vector<ll> results;
    for (auto& line: lines)
    {
        vector<char> stack;
        ll result = 0;
        bool ignore{false};
        for (auto& c: line)
        {
            if (find(opening.begin(), opening.end(), c) != opening.end())
            {
                stack.push_back(c); 
                continue;
            }
            
            if (stack.back() != match[c])
            {
                result += points[c];
                ignore = true;
                break;
            }

            stack.pop_back();
        }

        if (ignore or stack.empty()) continue;
        while (not stack.empty())
        {
            result *= 5;
            result += cpoints[cmatch[stack.back()]];
            stack.pop_back();
        }
        results.push_back(result);
    }
    sort(results.begin(), results.end());
    Print(results[results.size()/2]);

    return 0;
}
