#include "../utils.h"

struct Command
{
    string cmd;
    ll val;
};

int main()
{
    vector<Command> cmds;
    string token;
    ll val;
    while (cin >> token)
    {
        cin >> val;
        cmds.push_back({token, val});
    }

    // Part 1
    ll depth = 0;
    ll position = 0;
    for (auto [c, v]: cmds)
    {
        if (c == "forward") position += v;
        if (c == "up") depth += v;
        if (c == "down") depth -= v;
    }

    print(abs(depth) * position);

    // Part 2
    position = 0;
    depth = 0;
    ll aim = 0;
    for (auto [c, v]: cmds)
    {
        if (c == "forward")
        {
            position += v;
            depth += aim * v;
        }
        if (c == "up") aim += v;
        if (c == "down") aim -= v;
    }
    print(abs(depth) * position);

    return 0;
}
