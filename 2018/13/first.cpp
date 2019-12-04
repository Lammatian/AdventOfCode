#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <map>

struct Point {
    int x;
    int y;

    Point& operator+=(const Point& other) {
        this->x += other.x;
        this->y += other.y;
        return *this;
    }

    friend Point operator+(const Point& a, const Point& b) {
        return {a.x + b.x, a.y + b.y};
    }
};

enum class Direction {
    Up,
    Right,
    Down,
    Left
};

struct Car {
    Point pos;
    Direction dir;
    int turns;
    Point start;
};

class Compare {
public:
    bool operator()(const Car& a, const Car& b) {
        return a.pos.x > b.pos.x || a.pos.x == b.pos.x && a.pos.y > b.pos.y;
    }
};

enum class Road {
    None,
    StraightV,
    StraightH,
    Intersection,
    TurnRight,
    TurnLeft
};

struct Tile {
    Road road;
    bool car;
};

enum class Turn {
    Left,
    Right,
    Straight
};

std::map<Direction, Point> m2d = {
    {Direction::Up, {0, -1}},
    {Direction::Right, {1, 0}},
    {Direction::Down, {0, 1}},
    {Direction::Left, {-1, 0}}
};

std::map<int, Turn> turnRules = {
    {0, Turn::Left},
    {1, Turn::Straight},
    {2, Turn::Right}
};

std::map<Direction, Direction> next = {
    {Direction::Up, Direction::Right},
    {Direction::Right, Direction::Down},
    {Direction::Down, Direction::Left},
    {Direction::Left, Direction::Up}
};

std::map<Direction, Direction> prev = {
    {Direction::Up, Direction::Left},
    {Direction::Left, Direction::Down},
    {Direction::Down, Direction::Right},
    {Direction::Right, Direction::Up}
};

Direction turn(Direction d, Turn t) {
    if (t == Turn::Straight) {
        return d;
    } else if (t == Turn::Right) {
        return next[d];
    } else {
        return prev[d];
    }
}

int main() {
    std::ifstream f("input.txt");
    std::vector<std::vector<Tile>> roads;
    //std::vector<Car> cars;
    std::priority_queue<Car, std::vector<Car>, Compare> cars;
    std::string token;
    int row = 0;

    while (getline(f, token)) {
        roads.push_back(std::vector<Tile>());

        for (int i = 0; i < token.length(); ++i) {
            char c = token[i];

            switch (c) {
                case ' ':
                    roads.rbegin()->push_back({Road::None, false});
                    break;
                case '-':
                    roads.rbegin()->push_back({Road::StraightH, false});
                    break;
                case '|':
                    roads.rbegin()->push_back({Road::StraightV, false});
                    break;
                case '/':
                    roads.rbegin()->push_back({Road::TurnRight, false});
                    break;
                case '\\':
                    roads.rbegin()->push_back({Road::TurnLeft, false});
                    break;
                case '+':
                    roads.rbegin()->push_back({Road::Intersection, false});
                    break;
                case '>':
                    roads.rbegin()->push_back({Road::StraightH, true});
                    //cars.push_back({{i, row}, Direction::Right, 0});
                    cars.push({{i, row}, Direction::Right, 0, {i, row}});
                    break;
                case '<':
                    roads.rbegin()->push_back({Road::StraightH, true});
                    //cars.push_back({{i, row}, Direction::Left, 0});
                    cars.push({{i, row}, Direction::Left, 0, {i, row}});
                    break;
                case '^':
                    roads.rbegin()->push_back({Road::StraightV, true});
                    //cars.push_back({{i, row}, Direction::Up, 0});
                    cars.push({{i, row}, Direction::Up, 0, {i, row}});
                    break;
                case 'v':
                    roads.rbegin()->push_back({Road::StraightV, true});
                    //cars.push_back({{i, row}, Direction::Up, 0});
                    cars.push({{i, row}, Direction::Down, 0, {i, row}});
                    break;
                default:
                    std::cout << "Didn't consider " << c << std::endl;
                    break;
            }
        }

        row++;
    }

    int count = 1;
    std::priority_queue<Car, std::vector<Car>, Compare> newCars;

    while (true) {
        std::cout << "Round " << count << std::endl;
        newCars = std::priority_queue<Car, std::vector<Car>, Compare>();

        while (!cars.empty()) {
            Car car = cars.top();
            cars.pop();
            std::cout << "Car old pos: " << car.pos.x << ", " << car.pos.y << std::endl;
            std::cout << "Direction: ";

            if (car.dir == Direction::Up) {
                std::cout << "Up" << std::endl;
            } else if (car.dir == Direction::Right) {
                std::cout << "Right" << std::endl;
            } else if (car.dir == Direction::Down) {
                std::cout << "Down" << std::endl;
            } else {
                std::cout << "Left" << std::endl;
            }

            roads[car.pos.y][car.pos.x].car = false;
            car.pos += m2d[car.dir];
            std::cout << "Car new pos: " << car.pos.x << ", " << car.pos.y << std::endl;

            if (roads[car.pos.y][car.pos.x].car) {
                std::cout << car.pos.x << "," << car.pos.y << std::endl;
                return 0;
            }

            roads[car.pos.y][car.pos.x].car = true;

            // Change direction at turns and intersections
            switch (roads[car.pos.y][car.pos.x].road) {
                case Road::Intersection:
                    car.dir = turn(car.dir, turnRules[car.turns]);
                    car.turns = (car.turns + 1) % 3;
                    break;
                case Road::TurnRight:
                    if (car.dir == Direction::Up) {
                        car.dir = Direction::Right;
                    } else if (car.dir == Direction::Left) {
                        car.dir = Direction::Down;
                    } else if (car.dir == Direction::Right) {
                        car.dir = Direction::Up;
                    } else if (car.dir == Direction::Down) {
                        car.dir = Direction::Left;
                    } else {
                        std::cout << "Something weird happened at right turn" << std::endl;
                    }
                    break;
                case Road::TurnLeft:
                    if (car.dir == Direction::Up) {
                        car.dir = Direction::Left;
                    } else if (car.dir == Direction::Right) {
                        car.dir = Direction::Down;
                    } else if (car.dir == Direction::Down) {
                        car.dir = Direction::Right;
                    } else if (car.dir == Direction::Left) {
                        car.dir = Direction::Up;
                    } else {
                        std::cout << "Something weird happened at left turn" << std::endl;
                    }
                    break;
                case Road::None:
                    std::cout << "Car with start " << car.start.x << "," << car.start.y << " off road somehow" << std::endl;
                    return 0;
                    break;
                default:
                    std::cout << "Direction didn't change" << std::endl;
                    break;
            }

            newCars.push(car);
        }

        cars = newCars;
        count++;
    }

    return 0;
}