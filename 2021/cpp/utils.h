#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <sstream>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <utility>

using namespace std;
using ll = long long;

template<char delim = ' ', char end = '\n'>
void print()
{
    cout << "\b" << end;
}

template<char delim = ' ', char end = '\n', typename First>
void print(First&& first)
{
    cout << forward<First>(first) << end;
}

template<char delim = ' ', char end = '\n', typename First, typename ...Rest>
void print(First&& first, Rest&& ...rest)
{
    cout << forward<First>(first) << delim;
    print<delim, end>(forward<Rest>(rest)...);
}

template<char delim = ' ', typename Container>
void printCont(const Container& c)
{
    for (const auto& v: c)
    {
        print<delim, delim>(v);
    }
    print();
}

template<char delim = ' ', typename K, typename V>
void printMap(const unordered_map<K, V>& m)
{
    for (const auto& [k, v]: m)
    {
        print<':', delim>(k, v);
    }
    print();
}

vector<ll> readIntLine(istream& is)
{
    string token;
    getline(is, token);
    stringstream ss(token);
    ll val;
    vector<ll> result;
    while (ss >> val)
    {
        result.push_back(val);
    }
    return result;
}

vector<ll> readIntLine(istream& is, char delim)
{
    string token;
    getline(is, token);
    stringstream ss(token);
    vector<ll> result;
    while (getline(ss, token, delim))
    {
        result.push_back(stoll(token));
    }
    return result;
}

vector<ll> readInts(istream& is, char delim=' ')
{
    vector<ll> result;
    string token;
    while (getline(is, token, delim))
        result.push_back(stoll(token));

    return result;
}

ll btoll(const string& s)
{
    return stoll(s, nullptr, 2);
}

template<typename T>
vector<vector<T>> vec2d(int rows, int cols, T deft)
{
    vector<vector<T>> result(rows);
    for (int i = 0; i < rows; ++i)
    {
        result[i] = vector<T>(cols, deft);
    }
    return result;
}

template<typename T>
vector<vector<T>> vec2d(int rowcols, T deft)
{
    vector<vector<T>> result(rowcols);
    for (int i = 0; i < rowcols; ++i)
    {
        result[i] = vector<T>(rowcols, deft);
    }
    return result;
}
