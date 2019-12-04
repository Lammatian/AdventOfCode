//#define DBG
#ifdef DBG
#   define DEBUG(x) std::cout << x;
#else
#   define DEBUG(x) ;
#endif

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <queue>
#include <algorithm>
#include <cmath>
#include <unistd.h>

struct Point {
    int x;
    int y;
};

bool operator==(const Point& a, const Point& b) {
    return (a.x == b.x && a.y == b.y);
}

bool operator!=(const Point& a, const Point& b) {
    return (a.x != b.x || a.y != b.y);
}

bool higherReadOrder(const Point& a, const Point& b) {
    return a.y > b.y || (a.y == b.y && a.x > b.x);
}

bool lowerReadOrder(const Point& a, const Point& b) {
    return a.y < b.y || (a.y == b.y && a.x < b.x);
}

class CompareReadOrder {
public:
    bool operator()(const Point& a, const Point& b) {
        return a.y < b.y || (a.y == b.y && a.x < b.x);
    }
};

enum class Race {
    Elf,
    Goblin
};

struct Fighter {
    Race type;
    Point pos;
    int HP;
    int AD;
};

bool compareReadOrder(const Fighter* f, const Fighter* g) {
    return f->pos.y < g->pos.y || (f->pos.y == g->pos.y && f->pos.x < g->pos.x);
}

enum class Structure {
    Wall,
    Empty
};

struct Tile {
    Structure type;
    Fighter* fighter;
};

bool isNextToEnemy(const Fighter* f, const std::vector<std::vector<Tile>>& field, Point& p) {
    // if is next to enemy, save best position to attack in p
    int x = f->pos.x;
    int y = f->pos.y;
    int h = field.size();
    int w = field[0].size();
    int currentHP = 201;

    std::vector<Point> possible = {
        {x, y - 1},
        {x - 1, y},
        {x + 1, y},
        {x, y + 1}
    };

    for (auto &pt: possible) {
        if (pt.x >= 0 && pt.x < field[0].size() && pt.y >= 0 && pt.y < field.size()) {
            if (field[pt.y][pt.x].type == Structure::Empty) {
                if (field[pt.y][pt.x].fighter && field[pt.y][pt.x].fighter->type != f->type) {
                    if (field[pt.y][pt.x].fighter->HP < currentHP && field[pt.y][pt.x].fighter->HP > 0) {
                        currentHP = field[pt.y][pt.x].fighter->HP;
                        p = pt;
                    }
                }
            }
        }
    }

    return currentHP != 201;
}

struct PointPath {
    Point p;
    std::vector<Point> path;
};

std::vector<Point> getNeighbours(const Fighter* f, Point p, const std::vector<std::vector<Tile>>& field) {
    // Return neighbours in read order
    std::vector<Point> result;
    std::vector<Point> possible = {
        {p.x, p.y - 1},
        {p.x - 1, p.y},
        {p.x + 1, p.y},
        {p.x, p.y + 1}
    };

    for (auto &pt: possible) {
        if (pt.x >= 0 && pt.x < field[0].size() && pt.y >= 0 && pt.y < field.size()) {
            if (field[pt.y][pt.x].type == Structure::Empty) {
                if (!field[pt.y][pt.x].fighter) {
                    result.push_back(pt);
                } else if (field[pt.y][pt.x].fighter->type != f->type) {
                    result.push_back(pt);
                } else if (field[pt.y][pt.x].fighter->HP <= 0) {
                    result.push_back(pt);
                }
            }
        }
    }

    return result;
}

bool findClosestEnemy(const Fighter* f, const std::vector<std::vector<Tile>>& field, Point& p) {
    // Dijkstra: if there is an enemy reachable, save position to move in p
    //std::cout << "Finding closest enemy for fighter at " << f->pos.x << "," << f->pos.y << std::endl;
    Point start = f->pos;
    std::vector<std::vector<bool>> visited;
    // For debug
    std::stringstream ss;

    for (int i = 0; i < field.size(); ++i) {
        visited.push_back(std::vector<bool>());

        for (int j = 0; j < field[i].size(); ++j) {
            visited[i].push_back(false);
        }
    }

    std::queue<PointPath> q;
    q.push({start, {}});
    visited[start.y][start.x] = true;
    bool foundEnemy = false;
    int distToEnemy;
    Point enemyPoint;
    std::vector<Point> pathToEnemy;
    //std::vector<std::vector<Point>> paths;

    while (!q.empty()) {
        PointPath curr = q.front();
        q.pop();
        int dist = curr.path.size(); 
        int x = curr.p.x;
        int y = curr.p.y;

        ss.str(std::string());
        ss << "Considering point " << x << "," << y << " at distance " << dist << ": ";
        DEBUG(ss.str());

        bool hasCurrEnemy = field[y][x].fighter && field[y][x].fighter->type != f->type;

        ss.str(std::string());
        ss << (hasCurrEnemy ? "Point has enemy" : "No enemy") << std::endl;
        DEBUG(ss.str());

        if (foundEnemy && distToEnemy < dist) {
            // No paths that could lead to closer enemy
            break;
        } else if (foundEnemy && hasCurrEnemy && distToEnemy == dist) {
            if (lowerReadOrder(curr.path[0], pathToEnemy[0])) {
                pathToEnemy = curr.path;
            }

            // There is no point considering further ones from here
            continue;
        } 
        else if (!foundEnemy && hasCurrEnemy) {
            foundEnemy = true;
            distToEnemy = dist;
            enemyPoint = curr.p;
            pathToEnemy = curr.path;
            
            // There is no point considering further ones from here
            continue;
        }

        // Got here only if no enemy found and current has no enemy
        for (auto &pt: getNeighbours(f, curr.p, field)) {
            if (!visited[pt.y][pt.x]) {
                visited[pt.y][pt.x] = true;
                ss.str(std::string());
                ss << "Adding point " << pt.x << "," << pt.y << " to queue" << std::endl;
                DEBUG(ss.str());

                std::vector<Point> path = curr.path;
                path.push_back(pt);
                q.push({pt, path});
            }
        }
    }

    if (!foundEnemy) {
        ss.str(std::string());
        ss << "Didn't find enemy" << std::endl;
        DEBUG(ss.str());
        return false;
    } else {
        ss.str(std::string());
        ss << "Found a path" << std::endl;
        DEBUG(ss.str());
        
        p = pathToEnemy[0];
        return true;
    }
}

void printBoard(int turn, const std::vector<std::vector<Tile>>& field) {
    std::cout << std::endl << "Round: " << turn << std::endl;

    for (auto &row: field) {
        for (auto &t: row) {
            if (t.type == Structure::Wall) {
                std::cout << "#";
            } else if (t.fighter) {
                std::cout << (t.fighter->type == Race::Elf ? "E" : "G"); 
            } else {
                std::cout << ".";
            }
        }

        std::cout << std::endl;
    }

    std::cout << std::endl << std::endl;
}

void printBoard2(int turn, const std::vector<std::vector<Tile>>& field, const std::vector<Fighter*>& fighters) {
    std::cout << "Round: " << turn << std::endl;

    int f = 0;

    for (auto &row: field) {
        std::string health;
        for (auto &t: row) {
            if (t.type == Structure::Wall) {
                std::cout << "#";
            } else if (t.fighter) {
                std::string type = (t.fighter->type == Race::Elf ? "E" : "G");
                std::cout << type;

                while (fighters[f]->HP <= 0) {
                    f++;
                }
                health += (health.length() != 0 ? ", " : "") + type + "(" + std::to_string(fighters[f]->HP) + ")";
                f++;
            } else {
                std::cout << ".";
            }
        }

        std::cout << "\t" << health << std::endl;
    }
}

void saveBoard(std::ostream& o, int turn, const std::vector<std::vector<Tile>>& field, const std::vector<Fighter*>& fighters) {
    o << "Round: " << turn << std::endl;

    int f = 0;

    for (auto &row: field) {
        std::string health;
        for (auto &t: row) {
            if (t.type == Structure::Wall) {
                o << "#";
            } else if (t.fighter) {
                std::string type = (t.fighter->type == Race::Elf ? "E" : "G");
                o << type;

                while (fighters[f]->HP <= 0) {
                    f++;
                }
                health += (health.length() != 0 ? ", " : "") + type + "(" + std::to_string(fighters[f]->HP) + ")";
                f++;
            } else {
                o << ".";
            }
        }

        o << "\t" << health << std::endl;
    }
}

int solve(std::vector<std::vector<Tile>> field, std::vector<Fighter*> fighters, int elves, int goblins) {
    int turns = 0;
    bool done = false;
    int startElves = elves;

    while (elves > 0 && goblins > 0) {
        std::sort(fighters.begin(), fighters.end(), compareReadOrder);
        usleep(50000);
        //printBoard(turns, field);
        printBoard2(turns, field, fighters);

        for (auto &f: fighters) {
            //std::cout << "Fighter at " << f->pos.x << "," << f->pos.y << ", HP: " << f->HP << " : ";

            if (f->HP <= 0) {
                //std::cout << "Dead [*]" << std::endl;
                // Died this round, will never be forgotten [*]
                continue;
            }

            Point p;

            if (isNextToEnemy(f, field, p)) {
                //std::cout << "Next to enemy";
                // attack
                field[p.y][p.x].fighter->HP -= f->AD;

                // if HP of enemy <= 0 elves/goblins--
                if (field[p.y][p.x].fighter->HP <= 0) {
                    //std::cout << ", killed him";
                    if (field[p.y][p.x].fighter->type == Race::Elf) {
                        elves--;
                        done = true;
                    } else {
                        goblins--;
                        done = goblins == 0;
                    }

                    field[p.y][p.x].fighter = nullptr;
                }
            } else if (findClosestEnemy(f, field, p)) {
                //std::cout << "Moving to " << p.x << "," << p.y;
                // move
                Point temp = f->pos;
                f->pos = p;
                field[p.y][p.x].fighter = f;
                field[temp.y][temp.x].fighter = nullptr;

                // Allow attack straight after move
                if (isNextToEnemy(f, field, p)) {
                    //std::cout << "Next to enemy";
                    // attack
                    field[p.y][p.x].fighter->HP -= f->AD;

                    // if HP of enemy <= 0 elves/goblins--
                    if (field[p.y][p.x].fighter->HP <= 0) {
                        //std::cout << ", killed him";
                        if (field[p.y][p.x].fighter->type == Race::Elf) {
                            elves--;
                            done = elves == 0;
                        } else {
                            goblins--;
                            done = goblins == 0;
                        }

                        field[p.y][p.x].fighter = nullptr;
                    }
                }
            }

            //std::cout << std::endl;

            if (done) {
                int i = fighters.size() - 1;
                while (fighters[i]->HP <= 0) {
                    i--;
                }

                if (f == fighters[i]) {
                    turns++;
                }

                //std::cout << "Done!" << std::endl;
                break;
            }
        }

        if (done) {
            break;
        }
        
        turns++;
    }

    if (elves < startElves) {
        return -1;
    }

    int leftHP = 0;

    for (auto f: fighters) {
        leftHP += std::max(f->HP, 0);
    }

    return leftHP * turns;
}

int main(int argc, char* argv[]) {
    std::ifstream f("input" + std::string(argv[1]) + ".txt");
    std::ofstream o("output.txt");
    std::vector<std::vector<Tile>> field;
    std::vector<Fighter*> fighters;
    std::string token;

    int elves, goblins, y;
    int AD = 4;
    int result = 0;

    while (true) {
        elves = 0;
        goblins = 0;
        y = 0;
        field = std::vector<std::vector<Tile>>();
        fighters = std::vector<Fighter*>();
        f.clear();
        f.seekg(0);

        while (getline(f, token)) {
            field.push_back(std::vector<Tile>());

            for (int x = 0; x < token.length(); ++x) {
                char c = token[x];
                Fighter* f = nullptr;
                
                switch (c) {
                    case '#':
                        field.rbegin()->push_back({Structure::Wall, nullptr});
                        break;
                    case '.':
                        field.rbegin()->push_back({Structure::Empty, nullptr});
                        break;
                    case 'G':
                        goblins++;
                        f = new Fighter{Race::Goblin, {x, y}, 200, 3};
                        fighters.push_back(f);
                        field.rbegin()->push_back({Structure::Empty, f});
                        break;
                    case 'E':
                        elves++;
                        f = new Fighter{Race::Elf, {x, y}, 200, AD};
                        fighters.push_back(f);
                        field.rbegin()->push_back({Structure::Empty, f});
                        break;
                    default:
                        std::cout << "Error when parsing the board at char " << c << ", position " << x << "," << y << std::endl;
                        return 0;
                }
            }

            y++;
        }

        std::cout << "Considering AD " << AD << std::endl;
        result = solve(field, fighters, elves, goblins); 

        if (result > 0) {
            break;
        }
        AD++;
    }

    std::cout << result << std::endl;

    return 0;
}
