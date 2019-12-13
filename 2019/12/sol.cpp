#include <cmath>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

using ll = long long;

struct triple {
    ll x, y, z;

    ll& operator[](int i) {
        if (i == 0) {
            return x;
        } else if (i == 1) {
            return y;
        } else if (i == 2) {
            return z;
        } else {
            ll undef = 0;
            return undef;
        }
    }
    
    triple& operator+=(const triple& other) {
        x += other.x;
        y += other.y;
        z += other.z;
    }

    friend std::ostream& operator<<(std::ostream& o, triple t) {
        o << "<x=" << t.x << ", y=" << t.y << ", z=" << t.z << ">";
    }
};

triple operator+(const triple& a, const triple& b) {
    return triple{a.x + b.x, a.y + b.y, a.z + b.z};
}

struct Moon {
    triple pos;
    triple vel;
    
    Moon(const std::string& str) {
        std::stringstream ss(str);
        std::vector<ll> vals;

        std::string token;

        while (getline(ss, token, '=')) {
            if (vals.size() < 2) {
                getline(ss, token, ',');
            } else {
                getline(ss, token, '>');
            }

            vals.push_back(std::stoi(token));
        }

        pos = {vals[0], vals[1], vals[2]};
        vel = {0, 0, 0};
    }

    void adjustVel(std::vector<Moon> moons) {
        triple adjustment = {0, 0, 0};

        for (auto& m : moons) {
            for (int i = 0; i < 3; ++i) {
                if (this->pos[i] < m.pos[i]) {
                    adjustment[i] += 1;
                } else if (this->pos[i] > m.pos[i]) {
                    adjustment[i] -= 1;
                }
            }
        }

        vel += adjustment;
    }

    void move() {
        pos += vel;
    }

    ll energy() {
        return energyKin() * energyPot();
    }

    ll energyKin() {
        ll result = 0;

        for (int i = 0; i < 3; ++i) {
            result += std::abs(vel[i]);
        }

        return result;
    }

    ll energyPot() {
        ll result = 0;

        for (int i = 0; i < 3; ++i) {
            result += std::abs(pos[i]);
        }

        return result;
    }

    friend std::ostream& operator<<(std::ostream& o, Moon m) {
        o << "Moon |" << "pos: " << m.pos << " | vel: " << m.vel;
    }
};

ll sol1(std::vector<Moon> moons, ll steps=1000) {
    for (ll i = 0; i < steps; ++i) {
        for (auto& m : moons) {
            m.adjustVel(moons);
        }

        for (auto& m : moons) {
            m.move();
        }
    }

    ll result = 0;

    for (auto& m : moons) {
        result += m.energy();
    }

    return result;
}

ll gcd(ll a, ll b) {
    while (b != 0) {
        ll tmp = a;
        a = b;
        b = tmp % b;
    }

    return a;
}

ll lcm(ll a, ll b) {
    return a * b / gcd(a, b);
}

ll mlcm(std::vector<ll> nums) {
    ll result = nums[0];

    for (int i = 1; i < nums.size(); ++i) {
        result = lcm(result, nums[i]);
    }

    return result;
}

// pos and vel for each moon
using moons_pos = std::vector<std::pair<ll, ll>>;

moons_pos moons_to_pos(std::vector<Moon> moons, int dimension) {
    moons_pos result;

    for (auto& m : moons) {
        result.push_back({m.pos[dimension], m.vel[dimension]});
    }

    return result;
}

ll steps_to_repeat(std::vector<Moon> moons, int dimension) {
    std::set<moons_pos> positions;
    positions.insert(moons_to_pos(moons, dimension));
    
    ll steps = 1; // Account for step increase at end
    while (true) {
        for (auto& m : moons) {
            m.adjustVel(moons);
        }

        for (auto& m : moons) {
            m.move();
        }

        moons_pos current_pos = moons_to_pos(moons, dimension);
        
        if (positions.find(current_pos) != positions.end()) {
            return steps;
        } else {
            positions.insert(current_pos);
        }

        steps++;
    }
}

ll sol2(std::vector<Moon> moons) {
    std::vector<ll> repeat_times;

    for (int i = 0; i < 3; ++i) {
        repeat_times.push_back(steps_to_repeat(moons, i));
    }

    return mlcm(repeat_times);
}

int main(int argc, char* argv[]) {
    std::ifstream input(argv[1]);
    std::string line;
    std::vector<Moon> moons;

    if (!input.is_open()) {
        return 1;
    }

    while (getline(input, line)) {
        moons.push_back(Moon(line));
    }

    ll steps1 = 1000;

    if (argc > 2) {
        steps1 = std::stoi(argv[2]);
    }

    std::cout << sol1(moons, steps1) << std::endl;
    std::cout << sol2(moons) << std::endl;

    return 0;
}