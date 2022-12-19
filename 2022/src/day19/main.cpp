#include <iostream>
#include <vector>
#include <cmath>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <queue>
using namespace std;

int MINS1 = 24;
int MINS2 = 32;

ostream& operator<<(ostream& os, const vector<int>& v)
{
    os << "[";
    for (int i = 0; i < v.size(); ++i)
    {
        os << v[i];
        if (i != v.size() - 1)
            os << ", ";
    }
    os << "]";
    return os;
}

struct Test
{
    int mins;
    vector<int> M;
    vector<int> R;

};

bool operator==(const Test& t1, const Test& t2)
{
    return t1.mins == t2.mins && t1.M == t2.M && t1.R == t2.R;
}

bool operator<=(const Test& t1, const Test& t2)
{
    // Only compare the ones that have the same mins
    if (t1.M[0] > t2.M[0]) return false;
    if (t1.M[1] > t2.M[1]) return false;
    if (t1.M[2] > t2.M[2]) return false;
    if (t1.M[3] > t2.M[3]) return false;
    if (t1.R[0] > t2.R[0]) return false;
    if (t1.R[1] > t2.R[1]) return false;
    if (t1.R[2] > t2.R[2]) return false;
    if (t1.R[3] > t2.R[3]) return false;
    return true;
    //return t1.M <= t2.M && t1.R <= t2.R;
}

ostream& operator<<(ostream& os, const Test& t)
{
    os << t.mins << " " << t.M << " " << t.R;
    return os;
}

namespace std {
    template<>
    struct hash<vector<int>>
    {
        std::size_t operator()(const vector<int>& v) const
        {
            std::size_t seed = v.size();
            for(auto& i : v)
            {
                seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            }
            return seed;
        }
    };

  template <>
  struct hash<Test>
  {
    std::size_t operator()(const Test& k) const
    {
      using std::size_t;
      using std::hash;
      using std::string;

      // Compute individual hash values for first,
      // second and third and combine them using XOR
      // and bit shifting:

      return ((hash<int>()(k.mins)
               ^ (hash<vector<int>>()(k.M) << 1)) >> 1)
               ^ (hash<vector<int>>()(k.R) << 1);
    }
  };

}

unordered_map<Test, int> memo;

int rec(int mins, vector<int>& M, vector<int>& R, int co, int cc, vector<int> cb, vector<int> cg)
{
    if (mins == 0)
    {
        return M[3];
    }

    if (memo.find({mins, M, R}) != memo.end())
    {
        Test t{mins, M, R};
        return memo[t];
    }

    vector<int> newM{M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
    int best = 0;

    if (cg[0] <= M[0] and cg[1] <= M[2])
    {
        M = {M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
        M[0] -= cg[0];
        M[2] -= cg[1];
        R[3] += 1;
        best = max(best, rec(mins - 1, M, R, co, cc, cb, cg));
        M[0] += cg[0];
        M[2] += cg[1];
        R[3] -= 1;
        M = {M[0] - R[0], M[1] - R[1], M[2] - R[2], M[3] - R[3]};
        return best;
    }
    if (cb[0] <= M[0] and cb[1] <= M[1])
    {
        M = {M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
        M[0] -= cb[0];
        M[1] -= cb[1];
        R[2] += 1;
        best = max(best, rec(mins - 1, M, R, co, cc, cb, cg));
        M[0] += cb[0];
        M[1] += cb[1];
        R[2] -= 1;
        M = {M[0] - R[0], M[1] - R[1], M[2] - R[2], M[3] - R[3]};
        return best;
    }
    if (cc <= M[0] && R[1] < cb[1])
    {
        // At most cb[1] clay at once is useful
        M = {M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
        M[0] -= cc;
        R[1] += 1;
        best = max(best, rec(mins - 1, M, R, co, cc, cb, cg));
        M[0] += cc;
        R[1] -= 1;
        M = {M[0] - R[0], M[1] - R[1], M[2] - R[2], M[3] - R[3]};
    }
    if (co <= M[0] && R[0] < max({cc, cb[0], cg[0]}))
    {
        // At most max(cc, cb[0], cg[0]) ores at once are useful
        M = {M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
        M[0] -= co;
        R[0] += 1;
        best = max(best, rec(mins - 1, M, R, co, cc, cb, cg));
        M[0] += co;
        R[0] -= 1;
        M = {M[0] - R[0], M[1] - R[1], M[2] - R[2], M[3] - R[3]};
    }

    if (!(cb[0] <= M[0] and cb[1] <= M[1] and cg[0] <= M[0] and cg[1] <= M[2]))
    {
        // Only wait if cannot buy anything
        best = max(best,rec(mins - 1, newM, R, co, cc, cb, cg));
    }

    memo[{mins, M, R}] = best;
    return best;
}

int bfs(int mins_, vector<int>& M_, vector<int>& R_, int co, int cc, vector<int> cb, vector<int> cg)
{
    queue<pair<Test, Test>> q;
    q.push({{mins_+1, {0,0,0,0}, {0,0,0,0}}, {mins_, M_, R_}});
    vector<unordered_set<Test>> surv(mins_+3);
    int best = 0;

    while (!q.empty())
    {
        auto [tf, tt] = q.front(); q.pop();
        auto mins = tt.mins;
        auto M = tt.M;
        auto R = tt.R;

        if (tf.mins <= mins_ && tf.mins >= 0 && surv[tf.mins].find(tf) == surv[tf.mins].end())
        {
            // The 'from' wasn't best
            continue;
        }

        if (tt.mins == 0)
        {
            best = max(best, tt.M[3]);
            continue;
        }

        vector<Test> toerase;
        bool done = false;
        for (auto t: surv[tt.mins])
        {
            if (t <= tt)
            {
                toerase.push_back(t);
            }
            else if (tt <= t)
            {
                done = true;
                break;
            }
        }
        if (done) continue;
        if (!toerase.empty())
        {
            for (auto t: toerase)
            {
                surv[tt.mins].erase(t);
            }
        }
        surv[tt.mins].insert(tt);

        vector<int> newM{M[0] + R[0], M[1] + R[1], M[2] + R[2], M[3] + R[3]};
        int best = 0;
        if (!(cb[0] <= M[0] and cb[1] <= M[1] and cg[0] <= M[0] and cg[1] <= M[2]))
        {
            // Only wait if cannot buy anything
            Test newT = {mins - 1, newM, R};
            q.push({tt, newT});
        }

        if (cg[0] <= M[0] and cg[1] <= M[2])
        {
            auto newM = {M[0] + R[0] - cg[0], M[1] + R[1], M[2] + R[2] - cg[1], M[3] + R[3]};
            auto newR = {R[0], R[1], R[2], R[3] + 1};
            q.push({tt, {mins - 1, newM, newR}});
            continue;
        }
        if (cb[0] <= M[0] and cb[1] <= M[1])
        {
            auto newM = {M[0] + R[0] - cb[0], M[1] + R[1] - cb[1], M[2] + R[2], M[3] + R[3]};
            auto newR = {R[0], R[1], R[2] + 1, R[3]};
            q.push({tt, {mins - 1, newM, newR}});
            continue;
        }
        if (cc <= M[0] && R[1] < cb[1])
        {
            // At most cb[1] clay at once is useful
            auto newM = {M[0] + R[0] - cc, M[1] + R[1], M[2] + R[2], M[3] + R[3]};
            auto newR = {R[0], R[1] + 1, R[2], R[3]};
            q.push({tt, {mins - 1, newM, newR}});
        }
        if (co <= M[0] && R[0] < max({cc, cb[0], cg[0]}))
        {
            // At most max(cc, cb[0], cg[0]) ores at once are useful
            auto newM = {M[0] + R[0] - co, M[1] + R[1], M[2] + R[2], M[3] + R[3]};
            auto newR = {R[0] + 1, R[1], R[2], R[3]};
            q.push({tt, {mins - 1, newM, newR}});
        }
    }

    return best;
}

int main()
{
    string token;
    int i = 1;
    int result1 = 0;
    int result2 = 1;
    while (getline(cin, token))
    {
        cout << "Plan " << i << "\n";
        stringstream ss{token};
        vector<int> M(4);
        vector<int> R{1, 0, 0, 0};
        int co, cc;
        vector<int> cb(2);
        vector<int> cg(2);
        ss >> co >> cc >> cb[0] >> cb[1] >> cg[0] >> cg[1];
        //int res1 = rec(MINS1, M, R, co, cc, cb, cg);
        //int res2 = rec(MINS2, M, R, co, cc, cb, cg);
        int res1 = bfs(MINS1, M, R, co, cc, cb, cg);
        int res2 = bfs(MINS2, M, R, co, cc, cb, cg);
        cout << "Result of Plan " << i << ": " << res1 << ", " << res2 << "\n";
        result1 += i * res1;
        if (i <= 3)
            result2 *= res2;
        memo.clear();
        i++;
    }

    cout << result1 << "\n";
    cout << result2 << "\n";
    return 0;
}
