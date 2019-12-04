#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <cmath>

std::map<char, int> getState(std::vector<std::string> field, int x, int y) {
    std::map<char, int> result;

    int xstart = std::max(0, x - 1);
    int xend = std::min((int)field[0].size() - 1, x + 1);
    int ystart = std::max(0, y - 1);
    int yend = std::min((int)field[0].size() - 1, y + 1);

    for (int i = ystart; i <= yend; ++i) {
        for (int j = xstart; j <= xend; ++j) {
            result[field[i][j]]++;
        }
    }

    result[field[y][x]]--;

    return result;
}

int main() {
    std::fstream f("input.txt"); 
    std::vector<std::string> field;
    std::string token;

    while (getline(f, token)) {
        field.push_back(token);
    }

    int minutes = 0;
    int reps = 0;
    int firstrep = 0;
    int secondrep = 0;
    int cycleLen = 0;
    std::vector<std::string> repeatedField;
    std::set<std::vector<std::string>> maps;
    maps.insert(field);

    while (minutes < 5000) {
        std::vector<std::string> newField;

        for (int y = 0; y < field.size(); ++y) {
            std::string newRow("");

            for (int x = 0; x < field[0].size(); ++x) {
                auto state = getState(field, x, y);
                char newState;

                switch (field[y][x]) {
                    case '.':
                        if (state['|'] >= 3) {
                            newState = '|';
                        } else {
                            newState = '.';
                        }
                        break;
                    case '|':
                        if (state['#'] >= 3) {
                            newState = '#';
                        } else {
                            newState = '|';
                        }
                        break;
                    case '#':
                        if (state['#'] >= 1 && state['|'] >= 1) {
                            newState = '#';
                        } else {
                            newState = '.';
                        }
                        break;
                }

                newRow += newState;
            }

            newField.push_back(newRow);
        }

        field = newField;

        if (reps == 0 && maps.find(field) != maps.end()) {
            std::cout << "Minute " << minutes << " already happened" << std::endl;
            reps++;
            firstrep = minutes;
            repeatedField = field;
        } else if (reps == 1 && field == repeatedField) {
            std::cout << minutes << " already happened in minute " << firstrep << std::endl;
            cycleLen = (minutes - firstrep);
            secondrep = minutes;
            minutes++;
            break;
        }

        maps.insert(field);
        minutes++;
    }

    maps.clear();

    // We found the cycle length
    // just need to determine which one will it exactly be
    int cycle = secondrep - firstrep;
    int end = (secondrep + (1000000000 - firstrep) % cycle);
    std::cout << "Ending at " << end << " since " << end << " + " << cycle << "*k = 1000000000" << std::endl;

    while (minutes < end) {
        std::vector<std::string> newField;

        for (int y = 0; y < field.size(); ++y) {
            std::string newRow("");

            for (int x = 0; x < field[0].size(); ++x) {
                auto state = getState(field, x, y);
                char newState;

                switch (field[y][x]) {
                    case '.':
                        if (state['|'] >= 3) {
                            newState = '|';
                        } else {
                            newState = '.';
                        }
                        break;
                    case '|':
                        if (state['#'] >= 3) {
                            newState = '#';
                        } else {
                            newState = '|';
                        }
                        break;
                    case '#':
                        if (state['#'] >= 1 && state['|'] >= 1) {
                            newState = '#';
                        } else {
                            newState = '.';
                        }
                        break;
                }

                newRow += newState;
            }

            newField.push_back(newRow);
        }

        field = newField;
        minutes++;
    }

    int open = 0;
    int wood = 0;
    int lumber = 0;

    std::cout << "Final field:" << std::endl;
    for (auto &s: field) {
        std::cout << s << std::endl;
        open += std::count(s.begin(), s.end(), '.');
        wood += std::count(s.begin(), s.end(), '|');
        lumber += std::count(s.begin(), s.end(), '#');
    }

    std::cout << "Open: " << open << std::endl;
    std::cout << "Trees: " << wood << std::endl;
    std::cout << "Lumberyards: " << lumber << std::endl;
    std::cout << "Score: " << wood * lumber << std::endl;

    return 0;
}