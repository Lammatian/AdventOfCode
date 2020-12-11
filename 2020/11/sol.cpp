#include <iostream>
#include <fstream>
#include <functional>

#include "util.h"

typedef std::vector<std::string> Ferry;
typedef std::vector<std::vector<int>> OccupiedSeatCount;

struct point {
    int x, y;
};

typedef std::vector<point> VisibleSeats;
typedef std::vector<std::vector<std::vector<point>>> VisibleSeatMap;

void print_ferry(const Ferry& ferry) {
    std::cout << std::endl;
    for (auto& row: ferry) {
        for (auto& s: row) {
            std::cout << s;
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

bool in_bounds(int x, int y, int w, int h) {
    return 0 <= x && x < w && 0 <= y && y < h;
}

VisibleSeats get_adjacent_seats(const Ferry& ferry, int x, int y) {
    VisibleSeats result;
    int h = ferry.size();
    int w = ferry[0].size();

    for (int x_ = x - 1; x_ <= x + 1; ++x_) {
        for (int y_ = y - 1; y_ <= y + 1; ++y_) {
            if (x_ == x && y_ == y) {
                continue;
            }

            if (in_bounds(x_, y_, w, h) && ferry[y][x] != '.') {
                result.push_back({x_, y_});
            }
        }
    }

    return result;
}

int occupied_visible_seats(const Ferry& ferry,
                           const VisibleSeatMap& visible_seats,
                           int x,
                           int y) {
    int count = 0;

    for (auto& p: visible_seats[y][x]) {
        if (ferry[p.y][p.x] == '#') {
            count++;
        }
    }

    return count;
}

bool update_seats(Ferry& ferry, VisibleSeatMap& visible_seats, int empty_threshold) {
    bool changed = false;
    int h = ferry.size();
    int w = ferry[0].size();

    OccupiedSeatCount occ_count(h, std::vector<int>(w));
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            occ_count[y][x] = occupied_visible_seats(ferry, visible_seats, x, y);
        }
    }

    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            if (ferry[y][x] == 'L' && occ_count[y][x] == 0) {
                changed = true;
                ferry[y][x] = '#';
            } else if (ferry[y][x] == '#' && occ_count[y][x] >= empty_threshold) {
                changed = true;
                ferry[y][x] = 'L';
            }
        }
    }

    return changed;
}

VisibleSeats get_visible_seats(const Ferry& ferry, int x, int y) {
    VisibleSeats result;
    int h = ferry.size();
    int w = ferry[0].size();

    for (int dx = -1; dx <= 1; ++dx) {
        for (int dy = -1; dy <= 1; ++dy) {
            if (dx == 0 && dy == 0) {
                continue;
            }

            point cur = {x + dx, y + dy};

            while (in_bounds(cur.x, cur.y, w, h) && ferry[cur.y][cur.x] == '.') {
                cur.x += dx;
                cur.y += dy;
            }

            if (in_bounds(cur.x, cur.y, w, h)) {
                result.push_back(cur);
            }
        }
    }

    return result;
}

ll generic_sol(Ferry ferry,
               std::function<VisibleSeats (Ferry, int, int)> visible_seat_func,
               int empty_threshold) {
    int h = ferry.size();
    int w = ferry[0].size();
    VisibleSeatMap visible_seats(h, std::vector<VisibleSeats>(w));

    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            visible_seats[y][x] = visible_seat_func(ferry, x, y);
        }
    }

    while (update_seats(ferry, visible_seats, empty_threshold));

    ll result = 0;

    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            if (ferry[y][x] == '#') {
                result++;
            }
        }
    }

    return result;
}

int main(int argc, char** argv) {
    std::string filename = argc > 1 ? argv[1] : "input.txt";
    std::ifstream f(filename);

    Ferry ferry = util::readlines(f);

    std::cout << generic_sol(ferry, get_adjacent_seats, 4) << std::endl;
    std::cout << generic_sol(ferry, get_visible_seats, 5) << std::endl;

    return 0;
}