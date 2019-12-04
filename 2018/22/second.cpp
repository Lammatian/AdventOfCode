#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <map>
#include <cmath>
#include <algorithm>

enum class Tool {
    Torch,
    Gear,
    None
};

enum class Type {
    Rocky,
    Wet,
    Narrow
};

std::map<Type, std::vector<Tool>> allowedTools = {
    {Type::Rocky, {Tool::Torch, Tool::Gear}},
    {Type::Wet, {Tool::Gear, Tool::None}},
    {Type::Narrow, {Tool::Torch, Tool::None}}
};

struct Point {
    int x;
    int y;
    int time;
    Tool tool;
};

Point target = {10, 785};
//Point target = {10, 10};
int depth = 5616;
//int depth = 510;

int distance(const Point& p, const Point& q) {
    return std::abs(p.x - q.x) + std::abs(p.y - q.y);
}

class PointComparator {
public:
    bool operator()(const Point& a, const Point& b) {
        return a.time > b.time || (a.time == b.time && distance(a, target) > distance(b, target));
    }
};

bool isAllowed(Tool tool, const Point& p, const std::vector<std::vector<Type>>& board) {
    Type type = board[p.y][p.x];
    return std::find(allowedTools[type].begin(), allowedTools[type].end(), tool) != allowedTools[type].end();
}

std::vector<Point> getNeighbours(const Point& p, const std::vector<std::vector<Type>>& board) {
    // Get neighbours and time to get to them
    std::vector<Tool> tools = {
        Tool::None,
        Tool::Torch,
        Tool::Gear
    };
    std::vector<Point> moves = {
        {-1, 0},
        {1, 0},
        {0, 1},
        {0, -1}
    };
    std::vector<Point> result;
    
    for (auto &move: moves) {
        if (p.x + move.x >= 0 && p.y + move.y >= 0) {
            if (board[p.y][p.x] == Type::Rocky) {
                if (board[p.y + move.y][p.x + move.x] == Type::Narrow) {
                    if (p.tool == Tool::Torch) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::Torch});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::Torch});
                    }
                } else if (board[p.y + move.y][p.x + move.x] == Type::Rocky) {
                    result.push_back({p.x + move.x, p.y + move.y, p.time + 1, p.tool});
                } else if (board[p.y + move.y][p.x + move.x] == Type::Wet) {
                    if (p.tool == Tool::Torch) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::Gear});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::Gear});
                    }
                }
            } else if (board[p.y][p.x] == Type::Wet) {
                if (board[p.y + move.y][p.x + move.x] == Type::Narrow) {
                    if (p.tool == Tool::None) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::None});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::None});
                    }
                } else if (board[p.y + move.y][p.x + move.x] == Type::Wet) {
                    result.push_back({p.x + move.x, p.y + move.y, p.time + 1, p.tool});
                } else if (board[p.y + move.y][p.x + move.x] == Type::Rocky) {
                    if (p.tool == Tool::None) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::Gear});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::Gear});
                    }
                }
            } else if (board[p.y][p.x] == Type::Narrow) {
                if (board[p.y + move.y][p.x + move.x] == Type::Rocky) {
                    if (p.tool == Tool::Torch) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::Torch});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::Torch});
                    }
                } else if (board[p.y + move.y][p.x + move.x] == Type::Narrow) {
                    result.push_back({p.x + move.x, p.y + move.y, p.time + 1, p.tool});
                } else if (board[p.y + move.y][p.x + move.x] == Type::Wet) {
                    if (p.tool == Tool::Torch) {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 8, Tool::None});
                    } else {
                        result.push_back({p.x + move.x, p.y + move.y, p.time + 1, Tool::None});
                    }
                }
            }
        }
    }

    return result;
}

std::string t2s(Tool tool) {
    return (tool == Tool::Torch ? "torch" : (tool == Tool::Gear ? "gear" : "none"));
}

std::string b2s(Type type) {
    return (type == Type::Rocky ? "rocky" : (type == Type::Narrow ? "narrow" : "wet"));
}

Point findMin(const std::vector<std::vector<int>>& dN, const std::vector<std::vector<int>>& dT, const std::vector<std::vector<int>>& dG,
              const std::vector<std::vector<bool>>& vN, const std::vector<std::vector<bool>>& vT, const std::vector<std::vector<bool>>& vG) {
    int minDist = 999999;
    Point minPoint;

    for (int i = 0; i < dN.size(); ++i) {
        for (int j = 0; j < dN[0].size(); ++j) {
            if (dN[i][j] < minDist && !vN[i][j]) {
                minDist = dN[i][j];
                minPoint = {j, i, dN[i][j], Tool::None};
            }
            if (dT[i][j] < minDist && !vT[i][j]) {
                minDist = dT[i][j];
                minPoint = {j, i, dT[i][j], Tool::Torch};
            }
            if (dG[i][j] < minDist && !vG[i][j]) {
                minDist = dG[i][j];
                minPoint = {j, i, dG[i][j], Tool::Gear};
            }
        }
    }

    return minPoint;
}

int main(int argc, char* argv[]) {
    int ext = 40;

    std::vector<std::vector<int>> gidx(target.y + ext);
    std::vector<std::vector<int>> erosion(target.y + ext);
    std::vector<std::vector<Type>> board(target.y + ext);
    std::vector<std::vector<bool>> visitedNone(target.y + ext);
    std::vector<std::vector<bool>> visitedTorch(target.y + ext);
    std::vector<std::vector<bool>> visitedGear(target.y + ext);
    std::vector<std::vector<int>> distanceNone(target.y + ext);
    std::vector<std::vector<int>> distanceTorch(target.y + ext);
    std::vector<std::vector<int>> distanceGear(target.y + ext);

    for (int i = 0; i < target.y + ext; ++i) {
        gidx[i] = std::vector<int>(target.x + ext);
        erosion[i] = std::vector<int>(target.x + ext);
        board[i] = std::vector<Type>(target.x + ext);
        visitedNone[i] = std::vector<bool>(target.x + ext, false);
        visitedTorch[i] = std::vector<bool>(target.x + ext, false);
        visitedGear[i] = std::vector<bool>(target.x + ext, false);
        distanceNone[i] = std::vector<int>(target.x + ext, 999999);
        distanceTorch[i] = std::vector<int>(target.x + ext, 999999);
        distanceGear[i] = std::vector<int>(target.x + ext, 999999);
    }

    for (int y = 0; y < target.y + ext; ++y) {
        for (int x = 0; x < target.x + ext; ++x) {
            if (x == target.x && y == target.y) {
                gidx[y][x] = 0;
                erosion[y][x] = (gidx[y][x] + depth) % 20183;
            } else if (x == 0) {
                gidx[y][x] = 48271*y;
                erosion[y][x] = (gidx[y][x] + depth) % 20183;
            } else if (y == 0) {
                gidx[y][x] = 16807*x;
                erosion[y][x] = (gidx[y][x] + depth) % 20183;
            } else {
                gidx[y][x] = erosion[y][x - 1] * erosion[y - 1][x];
                erosion[y][x] = (gidx[y][x] + depth) % 20183;
            }
        }
    }

    for (int y = 0; y < target.y + ext; ++y) {
        for (int x = 0; x < target.x + ext; ++x) {
            erosion[y][x] %= 3;

            if (erosion[y][x] == 0) {
                board[y][x] = Type::Rocky;
            } else if (erosion[y][x] == 1) {
                board[y][x] = Type::Wet;
            } else {
                board[y][x] = Type::Narrow;
            }
        }
    }

    for (auto &row: board) {
        for (auto &t: row) {
            if (t == Type::Rocky) {
                std::cout << ".";
            } else if (t == Type::Narrow) {
                std::cout << "|";
            } else {
                std::cout << "=";
            }
        }
        std::cout << std::endl;
    }

    erosion.clear();
    gidx.clear();

    distanceTorch[0][0] = 0;
    distanceGear[0][0] = 7;
    int maxtime = (target.x + target.y) * 8;
    int count = 0;

    // BETWEEN 1068 AND 1074 NOT 1072

    while (!visitedTorch[target.y][target.x]) { // && count < 50) {
        Point top = findMin(distanceNone, distanceTorch, distanceGear, visitedNone, visitedTorch, visitedGear);

        if (top.tool == Tool::Torch) {
            visitedTorch[top.y][top.x] = true;
        } else if (top.tool == Tool::Gear) {
            visitedGear[top.y][top.x] = true;
        } else if (top.tool == Tool::None) {
            visitedNone[top.y][top.x] = true;
        }

        //std::cout << "Looking at point " << top.x << "," << top.y << " with time " << top.time << " and tool " << t2s(top.tool) << std::endl;
        if (top.y > 0 && top.y % 100 == 0) {
            std::cout << top.x << "," << top.y << std::endl;
        }

        if (top.x == target.x && top.y == target.y) {
            std::cout << "Shortest path: " << top.time << std::endl;
            std::cout << "Tool: " << (top.tool == Tool::Torch ? "torch" : "gear") << std::endl;
            return 0;
        }

        for (auto &p: getNeighbours(top, board)) {
            if (p.time > maxtime) {
                continue;
            }

            if (board[p.y][p.x] == Type::Rocky) {
                if (p.tool == Tool::Gear) {
                    if (!visitedGear[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceGear[p.y][p.x] = std::min(distanceGear[p.y][p.x], p.time);
                    }
                } else {
                    if (!visitedTorch[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceTorch[p.y][p.x] = std::min(distanceTorch[p.y][p.x], p.time);
                    }
                }
            } else if (board[p.y][p.x] == Type::Narrow) {
                if (p.tool == Tool::Torch) {
                    if (!visitedTorch[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceTorch[p.y][p.x] = std::min(distanceTorch[p.y][p.x], p.time);
                    }
                } else {
                    if (!visitedNone[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceNone[p.y][p.x] = std::min(distanceNone[p.y][p.x], p.time);
                    }
                }
            } else if (board[p.y][p.x] == Type::Wet) {
                if (p.tool == Tool::Gear) {
                    if (!visitedGear[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceGear[p.y][p.x] = std::min(distanceGear[p.y][p.x], p.time);
                    } else {
                        //std::cout << "Visited " << p.x << "," << p.y << std::endl;
                    }
                } else {
                    if (!visitedNone[p.y][p.x]) {
                        //std::cout << "Updating point " << p.x << "," << p.y << " with tool " << t2s(p.tool) << " and time " << p.time << std::endl;
                        distanceNone[p.y][p.x] = std::min(distanceNone[p.y][p.x], p.time);
                    }
                }
            }
        }

        count++;
    }

    return 0;
}