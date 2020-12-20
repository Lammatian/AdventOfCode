#include <iostream>
#include <fstream>
#include <cassert>
#include <regex>

#include "util.h"

using namespace std;

using Grid = vector<string>;
using TileBorders = vector<int>;

void print_grid(const Grid& g) {
    for (auto& row: g) {
        cout << row << endl;
    }
}

struct Tile {
    int id;
    Grid grid;
    TileBorders borders;
};

enum Direction {
    NORTH = 0,
    EAST = 1,
    SOUTH = 2,
    WEST = 3
};

int border_to_int(string border) {
    int left = 0, right = 0;

    for (int i = 0; i < border.size(); ++i) {
        right <<= 1;
        left <<= 1;

        if (border[i] == '#') {
            right++;
        }

        if (border[border.size() - i - 1] == '#') {
            left++;
        }
    }

    return min(right, left);
}

vector<int> borders_of_grid(const Grid& tile_grid) {
    vector<int> borders;
    // North
    borders.push_back(border_to_int(tile_grid.front()));

    string left_border, right_border;

    for (auto& row: tile_grid) {
        left_border += row.front();
        right_border += row.back();
    }

    // East
    borders.push_back(border_to_int(right_border));
    // South
    borders.push_back(border_to_int(tile_grid.back()));
    // West
    borders.push_back(border_to_int(left_border));

    return borders;
}

vector<Tile> prep(vector<string> lines) {
    vector<Tile> tiles;
    int idx = 0;
    regex tile_id_re("Tile (\\d+):");
    smatch matches;

    while (idx < lines.size()) {
        assert(lines[idx][0] == 'T');
        Tile tile;
        regex_search(lines[idx], matches, tile_id_re);
        tile.id = stoi(matches[1]);
        idx++;

        while (lines[idx] != "") {
            tile.grid.push_back(lines[idx++]);
        }

        tile.borders = borders_of_grid(tile.grid);

        tiles.push_back(tile);

        while (idx < lines.size() && lines[idx] == "") {
            idx++;
        }
    }

    return tiles;
}

vector<int> count_all_borders(const vector<Tile>& tiles) {
    vector<int> borders(1024, 0);

    for (auto& tile: tiles) {
        for (auto& border: tile.borders) {
            borders[border]++;
        }
    }

    return borders;
}

ll sol1(const vector<Tile>& tiles) {
    ll result = 1;
    auto border_count = count_all_borders(tiles);

    for (auto& tile: tiles) {
        // Borders that do not have a matching pair (ones) and the ones that do
        // have a matching pair (twos). If a tile has two ones and two twos, it
        // must be a corner
        int ones = 0, twos = 0;

        for (auto& border: tile.borders) {
            if (border_count[border] == 1) {
                ones++;
            } else {
                twos++;
            }
        }

        if (ones == 2 && twos == 2) {
            result *= tile.id;
        }
    }

    return result;
}

int sad_sqrt(int n) {
    int result = 0;

    while (result*result != n) {
        result++;
    }

    return result;
}

void rotate_right_borders(TileBorders& borders) {
    int prev = borders.back();

    for (int i = 0; i < borders.size(); ++i) {
        swap(prev, borders[i]);
    }
}

// Inplace clockwise rotate. Taken from
// https://www.geeksforgeeks.org/rotate-a-matrix-by-90-degree-in-clockwise-direction-without-using-any-extra-space/
void rotate_right_grid(Grid& grid) {
    int N = grid.size();

    // Traverse each cycle
    for (int i = 0; i < N / 2; i++) {
        for (int j = i; j < N - i - 1; j++) {
 
            // Swap elements of each cycle
            // in clockwise direction
            int temp = grid[i][j];
            grid[i][j] = grid[N - 1 - j][i];
            grid[N - 1 - j][i] = grid[N - 1 - i][N - 1 - j];
            grid[N - 1 - i][N - 1 - j] = grid[j][N - 1 - i];
            grid[j][N - 1 - i] = temp;
        }
    }
}

void rotate_right(Tile& tile) {
    rotate_right_borders(tile.borders); 
    rotate_right_grid(tile.grid);
}

void flipX_borders(TileBorders& borders) {
    swap(borders[0], borders[2]);
}

void flipX_grid(Grid& grid) {
    for (int i = 0; i < grid.size() / 2; ++i) {
        swap(grid[i], grid[grid.size() - 1 - i]);
    }
}

void flipX(Tile& tile) {
    flipX_borders(tile.borders);
    flipX_grid(tile.grid);
}

vector<function<void(Tile&)>> orientations = {
    rotate_right,
    rotate_right,
    rotate_right,
    rotate_right,
    flipX,
    rotate_right,
    rotate_right,
    rotate_right,
};

vector<function<void(Grid&)>> grid_orientations = {
    rotate_right_grid,
    rotate_right_grid,
    rotate_right_grid,
    rotate_right_grid,
    flipX_grid,
    rotate_right_grid,
    rotate_right_grid,
    rotate_right_grid,
};

// Get the first (topleft) corner tile
Tile get_corner_tile(vector<Tile>& tiles, const vector<int>& border_count) {
    for (auto& tile: tiles) {
        int ones = 0, twos = 0;

        for (auto& border: tile.borders) {
            if (border_count[border] == 1) {
                ones++;
            } else {
                twos++;
            }
        }

        if (ones == 2 && twos == 2) {
            for (auto& orient: orientations) {
                orient(tile);

                // North and west borders don't have matching ones
                if (border_count[tile.borders[NORTH]] == 1 && border_count[tile.borders[WEST]] == 1) {
                    return tile;
                }
            }
        }
    }
}

// Given a tile and a direction, find a border that matches this tile's border
// at the direction
Tile find_matching_tile(const vector<Tile>& tiles,
                        const Tile& target,
                        Direction direction) {
    for (auto& tile: tiles) {
        for (auto& border: tile.borders) {
            if (tile.id != target.id && border == target.borders[direction]) {
                return tile;
            }
        }
    }
}

// Find and orient a tile that will match the north and west tiles
Tile prepare_matching_tile(const vector<Tile>& tiles,
                           const vector<int>& border_count,
                           const Tile* north_tile,
                           const Tile* west_tile) {
    Tile matching;

    if (!north_tile) {
        matching = find_matching_tile(tiles, *west_tile, EAST);
    } else {
        matching = find_matching_tile(tiles, *north_tile, SOUTH);
    }

    for (auto& orient: orientations) {
        orient(matching);

        if (!north_tile) {
            if (matching.borders[WEST] == west_tile->borders[EAST]
                && border_count[matching.borders[NORTH]] == 1) {
                return matching;
            }
        } else if (!west_tile) {
            if (matching.borders[NORTH] == north_tile->borders[SOUTH]
                && border_count[matching.borders[WEST]] == 1) {
                return matching;
            }
        } else {
            if (matching.borders[NORTH] == north_tile->borders[SOUTH]
                && matching.borders[WEST] == west_tile->borders[EAST]) {
                return matching;
            }
        }
    }
}

// Get all the remaining tiles in the right places based on the topleft corner
// tile 
void fill_the_board(vector<vector<Tile>>& board,
                    const vector<Tile>& tiles,
                    int board_dims,
                    const vector<int>& border_count) {
    for (int y = 0; y < board_dims; ++y) {
        for (int x = 0; x < board_dims; ++x) {
            if (x == 0 && y == 0) {
                continue;
            }

            Tile* north_tile = nullptr;
            Tile* west_tile = nullptr;

            if (y - 1 >= 0) {
                north_tile = &board[y - 1][x];
            }

            if (x - 1 >= 0) {
                west_tile = &board[y][x - 1];
            }

            Tile matching = prepare_matching_tile(tiles, border_count, north_tile, west_tile);
            board[y][x] = matching;
        }
    }
}

// Given a board with properly oriented tiles, trim and concat them to get a
// full image
Grid get_trimmed_image(const vector<vector<Tile>>& board, int board_dims) {
    int tile_dims = board[0][0].grid.size();
    Grid full_image(board_dims * (tile_dims - 2), "");

    for (int y = 0; y < board_dims; ++y) {
        for (int x = 0; x < board_dims; ++x) {
            for (int r = 1; r < tile_dims - 1; ++r) {
                string tile_row = board[y][x].grid[r];
                full_image[y * (tile_dims - 2) + r - 1] += tile_row.substr(1, tile_row.size() - 2);
            }
        }
    }

    return full_image;
}

vector<pair<int, int>> MONSTER_PATTERN = {
    {18, 0},
    {0, 1},
    {5, 1},
    {6, 1},
    {11, 1},
    {12, 1},
    {17, 1},
    {18, 1},
    {19, 1},
    {1, 2},
    {4, 2},
    {7, 2},
    {10, 2},
    {13, 2},
    {16, 2}
};

int PATTERN_SIZE_X = 20;
int PATTERN_SIZE_Y = 3;

// Check if the pattern is visible given the topleft x and y coordinates
// The pattern is represented as a vector of points that have to be '#' or 'O',
// where 'O' represents the hashes that are already a part of some monster.
bool found_monster(const Grid& grid, int x, int y) {
    for (auto& p: MONSTER_PATTERN) {
        if (grid[y + p.second][x + p.first] != '#'
            && grid[y + p.second][x + p.first] != 'O') {
            return false;
        }
    }

    return true;
}

// Mark the monster whose pattern starts at (x, y) with Os
void mark_monster(Grid& full_image, int x, int y) {
    for (auto& p: MONSTER_PATTERN) {
        full_image[y + p.second][x + p.first] = 'O';
    }
}

// Given the image in some orientation, mark all the monsters appearing on it
void mark_all_monsters(Grid& full_image) {
    int image_dims = full_image.size();

    for (int x = 0; x <= image_dims - PATTERN_SIZE_X; ++x) {
        for (int y = 0; y <= image_dims - PATTERN_SIZE_Y; ++y) {
            if (found_monster(full_image, x, y)) {
                mark_monster(full_image, x, y);
            }
        }
    }
}

ll sol2(vector<Tile> tiles) {
    ll result = 0;
    int board_dims = sad_sqrt(tiles.size());
    auto border_count = count_all_borders(tiles);

    vector<vector<Tile>> board(board_dims, vector<Tile>(board_dims));
    board[0][0] = get_corner_tile(tiles, border_count);
    
    fill_the_board(board, tiles, board_dims, border_count);
    
    Grid full_image = get_trimmed_image(board, board_dims);
    int image_dims = full_image.size();

    // For each orientation mark monsters
    for (auto& orient: grid_orientations) {
        orient(full_image);
        mark_all_monsters(full_image);
    }

    for (auto& row: full_image) {
        for (auto& c: row) {
            if (c == '#') {
                result++;
            }
        }
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    vector<Tile> tiles = prep(util::readlines(f));

    cout << sol1(tiles) << endl;
    cout << sol2(tiles) << endl;

    return 0;
}