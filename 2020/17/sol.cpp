#include <iostream>
#include <fstream>
#include <algorithm>

#include "util.h"

using namespace std;

using Row = string;
using Slice = vector<Row>;
using Cube = vector<Slice>;
using Hypercube = vector<Cube>;

int SIZE = 21;

struct Point {
    int x, y, z, w;
};

tuple<Hypercube, ll> prep(const vector<string>& lines) {
    ll active_count = 0;
    Slice base_slice(SIZE, string(SIZE, '.'));
    Cube base_cube(SIZE, base_slice);
    Hypercube base_hypercube(SIZE, base_cube);

    int mid = SIZE / 2;
    int input_start = mid - lines.size() / 2;

    for (int i = 0; i < lines.size(); ++i) {
        for (int j = 0; j < lines[i].size(); ++j) {
            base_hypercube[mid][mid][input_start + i][input_start + j] = lines[i][j];

            if (lines[i][j] == '#') {
                active_count++;
            }
        }
    }

    return make_tuple(base_hypercube, active_count);
}

vector<Point> neighbours(Point p) {
    vector<Point> result;

    for (int w = -1; w <= 1; ++w) {
        for (int z = -1; z <= 1; ++z) {
            for (int y = -1; y <= 1; ++y) {
                for (int x = -1; x <= 1; ++x) {
                    if (w == 0 && z == 0 && y == 0 && x == 0) {
                        continue;
                    }

                    result.push_back({p.x + x, p.y + y, p.z + z, p.w + w});
                }
            }
        }
    }

    return result;
}

int active_neighbours(const Hypercube& hypercube, Point p) {
    vector<Point> ns = neighbours(p);

    return count_if(ns.begin(), ns.end(),
             [&hypercube](Point n) {
                 return hypercube[n.w][n.z][n.y][n.x] == '#';
             });
}

tuple<vector<Point>, vector<Point>> get_changes_3D(Hypercube& hypercube, int w) {
    ll active_change = 0;
    vector<Point> new_active;
    vector<Point> new_inactive;
    Cube& cube = hypercube[w];

    for (int z = 1; z < cube.size() - 1; ++z) {
        Slice slice = cube[z];
        for (int y = 1; y < slice.size() - 1; ++y) {
            Row row = slice[y];
            for (int x = 1; x < row.size() - 1; ++x) {
                int an = active_neighbours(hypercube, {x, y, z, w});
                if (row[x] == '#' && an != 2 && an != 3) {
                    new_inactive.push_back({x, y, z, w});
                    active_change--;
                } else if (row[x] == '.' && an == 3) {
                    new_active.push_back({x, y, z, w});
                    active_change++;
                }
            }
        }
    }

    return make_tuple(new_inactive, new_active);
}

void apply_updates(Hypercube& hypercube,
                   const vector<Point>& new_inactive,
                   const vector<Point>& new_active) {
    for (auto& p: new_inactive) {
        hypercube[p.w][p.z][p.y][p.x] = '.';
    }

    for (auto& p: new_active) {
        hypercube[p.w][p.z][p.y][p.x] = '#';
    }
}

ll update_active(Hypercube& hypercube, bool use_4D=false) {
    ll active_change = 0;
    vector<Point> new_inactive, new_active;

    if (use_4D) {
        for (int w = 1; w < hypercube.size() - 1; ++w) {
            vector<Point> cur_new_inactive, cur_new_active;
            tie(cur_new_inactive, cur_new_active) = get_changes_3D(hypercube, w);
            new_inactive.insert(new_inactive.end(), cur_new_inactive.begin(), cur_new_inactive.end());
            new_active.insert(new_active.end(), cur_new_active.begin(), cur_new_active.end());
        }
    } else {
        tie(new_inactive, new_active) = get_changes_3D(hypercube, hypercube.size() / 2);
    }

    apply_updates(hypercube, new_inactive, new_active);

    return new_active.size() - new_inactive.size();
}

ll sol1(Hypercube hypercube, ll active_count) {
    ll result = active_count;

    for (int i = 0; i < 6; ++i) {
        result += update_active(hypercube);
    }

    return result;
}

ll sol2(Hypercube hypercube, ll active_count) {
    ll result = active_count;

    for (int i = 0; i < 6; ++i) {
        result += update_active(hypercube, true);
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    Hypercube start_state;
    ll active_count;
    tie(start_state, active_count) = prep(util::readlines(f));

    cout << sol1(start_state, active_count) << endl;
    cout << sol2(start_state, active_count) << endl;

    return 0;
}