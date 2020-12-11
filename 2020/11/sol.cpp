#include <iostream>
#include <fstream>

#include "util.h"

typedef std::vector<std::string> Ferry;
typedef std::vector<std::vector<int>> OccupiedSeatCount;

struct point {
    int x, y;
};

typedef std::vector<point> VisibleSeats;
typedef std::vector<std::vector<std::vector<point>>> VisibleSeatMap;

bool in_bounds(int x, int y, int w, int h) {
    return 0 <= x && x < w && 0 <= y && y < h;
}

std::vector<point> adjacent(int x, int y, int w, int h) {
    std::vector<point> result;

    for (int x_ = x - 1; x_ <= x + 1; ++x_) {
        for (int y_ = y - 1; y_ <= y + 1; ++y_) {
            if (in_bounds(x_, y_, w, h) && (x_ != x || y_ != y)) {
                result.push_back({x_, y_});
            }
        }
    }

    return result;
}

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

bool update_seats(Ferry& ferry, OccupiedSeatCount& adj_count) {
    int h = ferry.size();
    int w = ferry[0].size();
    bool changed = false;

    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            if (ferry[y][x] == 'L' && adj_count[y][x] == 0) {
                changed = true;
                ferry[y][x] = '#';
            } else if (ferry[y][x] == '#' && adj_count[y][x] >= 4) {
                changed = true;
                ferry[y][x] = 'L';
            }
        }
    }

    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            int count = 0;
            for (auto& p: adjacent(x, y, w, h)) {
                if (ferry[p.y][p.x] == '#') {
                    count++;
                } 
            }

            adj_count[y][x] = count;
        }
    }

    return changed;
}

ll sol1(Ferry ferry) {
    int h = ferry.size();
    int w = ferry[0].size();
    OccupiedSeatCount adj_count;

    for (int i = 0; i < h; ++i) {
        adj_count.push_back(std::vector<int>(w, 0));
    }

    while(update_seats(ferry, adj_count));

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

bool update_seats2(Ferry& ferry,
                   const VisibleSeatMap& visible_seats) {
    bool changed = false;
    int h = ferry.size();
    int w = ferry[0].size();
    OccupiedSeatCount occ_count;

    for (int y = 0; y < h; ++y) {
        occ_count.push_back(std::vector<int>(w));

        for (int x = 0; x < w; ++x) {
            occ_count[y][x] = occupied_visible_seats(ferry, visible_seats, x, y);
        }
    }

    for (int x = 0; x < w; ++x) {
        for (int y = 0; y < h; ++y) {
            if (ferry[y][x] == '#' && occ_count[y][x] >= 5) {
                changed = true;
                ferry[y][x] = 'L';
            } else if (ferry[y][x] == 'L' && occ_count[y][x] == 0) {
                changed = true;
                ferry[y][x] = '#';    
            }
        }
    }

    return changed;
}

ll sol2(Ferry ferry) {
    int h = ferry.size();
    int w = ferry[0].size();
    VisibleSeatMap visible_seats;

    for (int y = 0; y < h; ++y) {
        visible_seats.push_back(std::vector<VisibleSeats>(w));

        for (int x = 0; x < w; ++x) {
            visible_seats[y][x] = get_visible_seats(ferry, x, y);
        }
    }

    while (update_seats2(ferry, visible_seats));

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

    std::cout << sol1(ferry) << std::endl;
    std::cout << sol2(ferry) << std::endl;

    return 0;
}