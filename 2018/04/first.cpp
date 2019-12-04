#include <iostream>
#include <algorithm>
#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>
#include <sstream>

struct GuardInfo {
    int slept;
    std::vector<int> sleep;
};

int main() {
    std::fstream f("input.txt");
    std::vector<std::string> info;
    std::string token;

    while (getline(f, token)) {
        info.push_back(token);
    }

    std::sort(info.begin(), info.end());
    std::unordered_map<int, GuardInfo> guards;
    
    int currentID = 0;
    int napStart = 0;
    int napEnd = 0;
    int mostSlept = 0;
    int mostSleptID = 0;

    for (int i = 0; i < info.size(); ++i) {
        if (info[i].find('#') != std::string::npos) {
            std::stringstream ss(info[i]);
            getline(ss, token, '#');
            getline(ss, token, ' ');
            currentID = std::stoi(token);

            if (guards.find(currentID) == guards.end()) {
                guards[currentID] = {0, std::vector<int>(60)};
            }

            i++;

            while (i < info.size() && info[i].find('#') == std::string::npos) {
                ss = std::stringstream(info[i]);
                getline(ss, token, ':');
                getline(ss, token, ']');

                if (info[i].find('f') != std::string::npos) {
                    napStart = std::stoi(token);
                } else {
                    napEnd = std::stoi(token);
                    guards[currentID].slept += napEnd - napStart;
                    
                    if (guards[currentID].slept > mostSlept) {
                        mostSlept = guards[currentID].slept;
                        mostSleptID = currentID;
                    }
                    
                    for (int j = napStart; j < napEnd; ++j) {
                        guards[currentID].sleep[j]++;
                    }
                }

                i++;
            }

            i--;
        }
    }

    std::cout << mostSleptID << std::endl;

    std::cout << std::distance(guards[mostSleptID].sleep.begin(), 
                               std::max_element(guards[mostSleptID].sleep.begin(), guards[mostSleptID].sleep.end())) << std::endl;

    return 0;
}