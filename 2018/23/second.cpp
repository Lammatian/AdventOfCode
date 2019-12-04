#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

typedef long long ll;

struct Position {
    ll x;
    ll y;
    ll z; 
    ll r;
};

ll distance(const Position& a, const Position& b) {
    return std::abs(a.x - b.x) + std::abs(a.y - b.y) + std::abs(a.z - b.z);
}

int inRange(const std::vector<Position>& pos, const Position& p, ll a) {
    int result = 0;

    for (auto &pt: pos) {
        if ((distance(pt, p) - pt.r)/a <= 0) {
            result++;
        }
    }

    return result;
}

std::ostream& operator<<(std::ostream& o, Position p) {
    o << p.x << "," << p.y << "," << p.z << " r: " << p.r;
}

int main() {
    ll approx = 10000000;
    std::fstream f("input.txt");
    std::vector<Position> pos;
    Position start{0, 0, 0, 0};
    ll maxx = 0;
    ll minx = 0;
    ll maxy = 0;
    ll miny = 0;
    ll maxz = 0;
    ll minz = 0;
    std::string token;

    while (getline(f, token, '<')) {
        getline(f, token, ',');
        ll x = std::stoll(token);
        maxx = std::max(maxx, x);
        minx = std::min(minx, x);
        getline(f, token, ',');
        ll y = std::stoll(token);
        maxy = std::max(maxy, y);
        miny = std::min(miny, y);
        getline(f, token, '>');
        ll z = std::stoll(token);
        maxz = std::max(maxz, z);
        minz = std::min(minz, z);
        getline(f, token, '=');
        getline(f, token);
        ll r = std::stoll(token);
        pos.push_back({x, y, z, r});
    }

    // start
    //approx = 1000000;
    //ll axs = minx;
    //ll axe = maxx;
    //ll ays = miny;
    //ll aye = maxy;
    //ll azs = minz;
    //ll aze = maxz;
    // 1st approx
    //approx = 100000;
    //ll axs = 50000000/approx;
    //ll axe = 70000000/approx;
    //ll ays = 35000000/approx;
    //ll aye = 55000000/approx;
    //ll azs = 40000000/approx;
    //ll aze = 60000000/approx;
    // 2nd approx
    // approx = 100000;
    //ll axs = 55000000/approx;
    //ll axe = 65000000/approx;
    //ll ays = 42000000/approx;
    //ll aye = 52000000/approx;
    //ll azs = 47000000/approx;
    //ll aze = 57000000/approx;
    //568,484,534
    // 3rd approx
    // approx = 10000;
    //ll axs = 56000000/approx;
    //ll axe = 57600000/approx;
    //ll ays = 47600000/approx;
    //ll aye = 49200000/approx;
    //ll azs = 52600000/approx;
    //ll aze = 54200000/approx;
    //5673,4847,5343
    // 4th approx
    //approx = 1000;
    //ll axs = 56600000/approx;
    //ll axe = 56900000/approx;
    //ll ays = 48300000/approx;
    //ll aye = 48600000/approx;
    //ll azs = 53300000/approx;
    //ll aze = 53600000/approx;
    //56723,48482,53439
    // 5th approx
    //approx = 100;
    //ll axs = 56710000/approx;
    //ll axe = 56730000/approx;
    //ll ays = 48470000/approx;
    //ll aye = 48490000/approx;
    //ll azs = 53420000/approx;
    //ll aze = 53450000/approx;
    //567215,484831,534407
    // 6th approx
    //approx = 10;
    //ll axs = 56720000/approx;
    //ll axe = 56723000/approx;
    //ll ays = 48481500/approx;
    //ll aye = 48484500/approx;
    //ll azs = 53439200/approx;
    //ll aze = 53442200/approx;
    //5672151,4848317,5344080
    // 7th approx
    //approx = 1;
    //ll axs = 56721400/approx;
    //ll axe = 56721600/approx;
    //ll ays = 48483100/approx;
    //ll aye = 48483300/approx;
    //ll azs = 53440700/approx;
    //ll aze = 53440900/approx;
    Position bestPos = {-minx -1, -miny-1, -minz-1, 0};
    int bestCount = 0;
    approx = 1;
    while (approx < std::max(maxx - minx, std::max(maxy - miny, maxz - minz))) {
        approx *= 2;
    }

    std::vector<ll> xs = {minx, maxx};
    std::vector<ll> ys = {miny, maxy};
    std::vector<ll> zs = {minz, maxz};

    while (true) {
        bestPos = {-minx-1, -miny-1, -minz-1, 0};
        bestCount = 0;
        for (int i = xs[0]; i <= xs[1]; i += approx) {
            for (int j = ys[0]; j <= ys[1]; j += approx) {
                for (int k = zs[0]; k <= zs[1]; k += approx) {
                    if (approx > maxx - minx) {
                        std::cout << i << "," << j << "," << k << std::endl;
                    }
                    int count = inRange(pos, {i, j, k, 0}, approx);
                    if (count > bestCount) {
                        bestCount = count;
                        bestPos = {i, j, k, 0};
                    } else if (count == bestCount) {
                        if (distance(start, {i, j, k, 0}) < distance(start, bestPos)) {
                            std::cout << "Updated best pos to " << i << "," << j << "," << k << std::endl;
                            bestPos = {i, j, k, 0};
                        }
                    }
                }
            }
        }

        if (approx == 1) {
            std::cout << bestPos << std::endl;
            std::cout << bestPos.x + bestPos.y + bestPos.z << std::endl;
            return 0;
        } else {
            xs = {bestPos.x - approx, bestPos.x + approx};
            ys = {bestPos.y - approx, bestPos.y + approx};
            zs = {bestPos.z - approx, bestPos.z + approx};
            approx /= 2;
        }

        if (approx*2 > maxx - minx) {
            std::cout << "First" << std::endl;
        }
        std::cout << bestPos << "[" << zs[0] << "," << zs[1] << "]" << std::endl;
    }

    //for (int i = axs; i <= axe; ++i) {
    //    if (i % 100 == 0) std::cout << i << std::endl;
    //    for (int j = ays; j <= aye; ++j) {
    //        for (int k = azs; k <= aze; ++k) {
    //            int count = inRange(pos, {i, j, k, 0});
    //            //std::cout << count << std::endl;
    //            if (count > bestCount) {
    //                countMax = 1;
    //                bestCount = count;
    //                bestPos = {i, j, k, 0};
    //            } else if (count == bestCount) {
    //                if (distance(start, {i, j, k, 0}) < distance(start, bestPos)) {
    //                    bestPos = {i, j, k, 0};
    //                }
    //                countMax++;
    //            }
    //        }
    //    }
    //}

    //std::cout << bestCount << std::endl;
    //std::cout << bestPos << std::endl;
    //std::cout << countMax << std::endl;

    return 0;
}