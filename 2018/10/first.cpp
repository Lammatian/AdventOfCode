#include <iostream>
#include <fstream>
#include <vector>
#include <string>

void draw(const std::vector<std::pair<int, int>>& pos, int minx, int maxx, int miny, int maxy) {
    std::vector<std::string> drawing(maxy - miny + 1);

    for (int i = 0; i < drawing.size(); ++i) {
        drawing[i] = std::string(maxx - minx + 1, ' ');
    }
    for (auto &x: pos) {
        std::cout << "(" << x.first << ", " << x.second << ")" << std::endl;
        drawing[x.second - miny][x.first - minx] = '#';
    }

    std::ofstream o("output.txt");
    for (auto &s: drawing) {
        std::cout << s << std::endl;
        o << s << std::endl;
    }
}

int main() {
    std::fstream f("input.txt");
    std::string token;
    std::vector<std::pair<int, int>> positions;
    std::vector<std::pair<int, int>> velocities;

    while (getline(f, token, '<')) {
        getline(f, token, ',');
        int a = std::stoi(token);
        getline(f, token, '>');
        int b = std::stoi(token);
        positions.push_back(std::make_pair(a, b));

        getline(f, token, '<');
        getline(f, token, ',');
        a = std::stoi(token);
        getline(f, token, '>');
        b = std::stoi(token);
        velocities.push_back(std::make_pair(a, b));
    }

    int minx = 50000;
    int maxx = -50000;
    int miny = 50000;
    int maxy = -50000;

    int start = 10086;

    for (int i = 0; i < positions.size(); ++i) {
        positions[i].first += start*velocities[i].first;
        minx = std::min(minx, positions[i].first);
        maxx = std::max(maxx, positions[i].first);
        positions[i].second += start*velocities[i].second;
        miny = std::min(miny, positions[i].second);
        maxy = std::max(maxy, positions[i].second);
    }

    draw(positions, minx, maxx, miny, maxy);

    //for (int s = 10071; s < 10090; ++s) {
    //    for (int i = 0; i < positions.size(); ++i) {
    //        positions[i].first += velocities[i].first;
    //        minx = std::min(minx, positions[i].first);
    //        maxx = std::max(maxx, positions[i].first);
    //        positions[i].second += velocities[i].second;
    //        miny = std::min(miny, positions[i].second);
    //        maxy = std::max(maxy, positions[i].second);
    //    }
    //    draw(positions, minx, maxx, miny, maxy);
    //}


    return 0;
}