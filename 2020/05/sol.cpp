#include <iostream>
#include <vector>
#include <algorithm>

#include "util.h"

ll seat_to_number(std::string seat) {
    ll result = 0;

    for (int i = 0; i < seat.size(); ++i) {
        char cur = seat[seat.size() - i - 1];

        if (cur == 'R' || cur == 'B') {
            result += 1 << i;
        }
    }

    return result;
}

ll sol1(std::vector<std::string> lines) {
    std::vector<ll> ids(lines.size());
    std::transform(lines.begin(), lines.end(), ids.begin(),
                   seat_to_number);

    return *std::max_element(ids.begin(), ids.end());
}

ll sol2(std::vector<std::string> lines) {
    std::vector<ll> ids(lines.size());
    std::transform(lines.begin(), lines.end(), ids.begin(),
                   seat_to_number);
    std::sort(ids.begin(), ids.end());

    ll last = ids[0] - 1;
    // Find first two consecutive elements differing by two
    auto it = std::find_if(ids.begin(), ids.end(),
                           [&last](ll i) {
                               bool diff = i - last == 2;
                               last = i;
                               return diff;
                           });

    return *it - 1;              
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    std::vector<std::string> lines = util::readlines(f);

    std::cout << sol1(lines) << std::endl;
    std::cout << sol2(lines) << std::endl;

    return 0;
}