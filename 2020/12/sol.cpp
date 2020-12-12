#include <iostream>
#include <fstream>
#include <map>

#include "util.h"

typedef std::vector<std::string> Commands;

std::map<char, std::pair<int, int>> dir2move = {
    {'N', {0, -1}},
    {'E', {1, 0}},
    {'S', {0, 1}},
    {'W', {-1, 0}}
};

std::map<int, char> angle2dir = {
    {0, 'N'},
    {90, 'E'},
    {180, 'S'},
    {270, 'W'}
};

class Ferry {
  public:
    int cur_angle;
    ll x = 0, y = 0;

    Ferry(int angle) : cur_angle{angle} {};

    void execute(std::string command) {
        if (command[0] == 'F') {
            command[0] = cur_dir();
        }

        if (command[0] == 'L' || command[0] == 'R') {
            int turn_angle = (command[0] == 'R' ? 1 : -1) * std::stoi(command.substr(1));
            cur_angle += turn_angle;
            cur_angle = (cur_angle + 360) % 360;
        } else {
            int mult = std::stoi(command.substr(1));
            x += mult * dir2move[command[0]].first;
            y += mult * dir2move[command[0]].second;
        }
    }

    char cur_dir() {
        return angle2dir[cur_angle];
    }
};

ll sol1(Commands commands) {
    // 90 for East
    Ferry f(90);

    for (auto& command: commands) {
        f.execute(command);
    }

    return std::abs(f.x) + std::abs(f.y);
}

class WaypointFerry {
  public:
    ll wx = 10, wy = -1;
    ll x = 0, y = 0;

    WaypointFerry() {};

    void execute(std::string command) {
        int val = std::stoi(command.substr(1));

        if (command[0] == 'F') {
            x += wx * val;
            y += wy * val;
        }

        if (command[0] == 'L') {
            int rots = val / 90;

            for (int i = 0; i < rots; ++i) {
                std::swap(wx, wy);
                wy = -wy;
            }
        } else if (command[0] == 'R') {
            int rots = val / 90;

            for (int i = 0; i < rots; ++i) {
                std::swap(wx, wy);
                wx = -wx;
            }
        } else {
            wx += val * dir2move[command[0]].first;
            wy += val * dir2move[command[0]].second;
        }
    }
};

ll sol2(Commands commands) {
    WaypointFerry f;

    for (auto& command: commands) {
        f.execute(command);
    }

    return std::abs(f.x) + std::abs(f.y);
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<std::string> commands = util::readlines(f);

    std::cout << sol1(commands) << std::endl;
    std::cout << sol2(commands) << std::endl;

    return 0;
}