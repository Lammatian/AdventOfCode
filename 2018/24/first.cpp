#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <vector>
#include <string>
#include <algorithm>

struct Army {
    int units;
    int unitHP;
    std::vector<std::string> weaknesses;
    std::vector<std::string> strengths;
    int AD;
    std::string type;
    int initiative;
};

std::ostream& operator<<(std::ostream& o, const Army& a) {
    o << "units: " << a.units << ", HP: " << a.unitHP << ", AD: " << a.AD << ", init: " << a.initiative;

    if (!a.weaknesses.empty()) {
        o << std::endl << "weaknesses: ";

        for (auto &w: a.weaknesses) {
            o << w << " ";
        }
    }

    if (!a.strengths.empty()) {
        o << std::endl << "strengths: ";

        for (auto &s: a.strengths) {
            o << s << " ";
        }
    }

    return o;
}

struct IIS { // initiative, index, side
    int initiative;
    int index;
    int side; // 0 - immune, 1 - infection
};

bool INComparator(const IIS& a, const IIS& b) {
    // initiative comparator
    // initiatives are distinct
    return a.initiative > b.initiative;
}

std::vector<std::string> parse(std::string s) {
    std::stringstream ss(s);
    std::string token;
    std::vector<std::string> result;

    if (s == "") {
        return result;
    }

    while (getline(ss, token, ',')) {
        result.push_back(token);
        getline(ss, token, ' ');
    }

    return result;
}

int effectivePower(const Army& a) {
    return a.AD * a.units;
}

bool EPComparator(const Army& a, const Army& b) {
    // better effective power or in case of a tie
    // better initiative
    return effectivePower(a) > effectivePower(b) ||
           effectivePower(a) == effectivePower(b) && a.initiative > b.initiative;
}

int calculateDamage(const Army& attacker, const Army& defender) {
    if (std::find(defender.strengths.begin(), defender.strengths.end(), attacker.type) != defender.strengths.end()) {
        return 0;
    } else if (std::find(defender.weaknesses.begin(), defender.weaknesses.end(), attacker.type) != defender.weaknesses.end()) {
        return 2*effectivePower(attacker);
    } else {
        return effectivePower(attacker);
    }
}

void attack(const Army& attacker, Army& defender) {
    int attackStrength = calculateDamage(attacker, defender);
    std::cout << std::endl << attacker << std::endl << "attacking for " << attackStrength << std::endl << defender << std::endl << std::endl;

    defender.units -= attackStrength/defender.unitHP;
}

int chooseTarget(Army attacker, std::vector<Army>& opp, const std::vector<int>& chosen) {
    int bestDamage = 0;
    int bestArmyID = -1;
    // initiative to 100
    Army bestArmy = {0, 0, {}, {}, 0, {}, 100};

    for (int i = 0; i < opp.size(); ++i) {
        if (std::find(chosen.begin(), chosen.end(), i) != chosen.end()) {
            // ignore already chosen
            continue;
        }

        int damage = calculateDamage(attacker, opp[i]);

        if (damage > bestDamage) {
            // better
            bestArmyID = i;
            bestDamage = damage;
            bestArmy = opp[i];
        } else if (damage == bestDamage) {
            // check if better
            if (EPComparator(opp[i], bestArmy)) {
                bestArmyID = i;
                bestDamage = damage;
                bestArmy = opp[i];
            }
        }
    }

    return bestArmyID;
}

std::pair<std::vector<std::string>, std::vector<std::string>> parseWS(std::string s) {
    std::vector<std::string> weak;
    std::vector<std::string> strong;
    std::string weakStr;
    std::string strongStr;

    if (s == "") {
        return std::make_pair(strong, weak);
    }

    std::string start = s.substr(2, 6);
    std::cout << "Start: " << start << std::endl;

    if (start.find("weak") != std::string::npos) {
        if (s.find(";") != std::string::npos) {
            // there is both with weak first
            weakStr = s.substr(10, s.find(";") - 10);
            strongStr = s.substr(s.find(";") + 12);
            strongStr = strongStr.substr(0, strongStr.length() - 1);
        } else {
            // there is only weak
            weakStr = s.substr(10, s.length() - 11);
        }
    } else if (start.find("immune") != std::string::npos) {
        if (s.find(";") != std::string::npos) {
            // there is both with immune first
            strongStr = s.substr(12, s.find(";") - 12);
            weakStr = s.substr(s.find(";") + 10);
            weakStr = weakStr.substr(0, weakStr.length() - 1);
        } else {
            // there is only strong
            strongStr = s.substr(12, s.length() - 13);
        }
    }

    weak = parse(weakStr);
    strong = parse(strongStr);

    return std::make_pair(strong, weak);
}

int main(int argc, char* argv[]) {
    std::fstream f("input" + std::string(argv[1]) + ".txt");
    std::string token;
    bool isInfection = false;
    std::vector<Army> immune;
    std::vector<Army> infection;
    std::regex parser("([0-9]+) units each with ([0-9]+) hit points( \\((immune to ([^;)]*))?(; )?(weak to ([^)]*))?\\))? with an attack that does ([0-9]*) (\\S+) damage at initiative ([0-9]+)");
    std::smatch matches;

    while (getline(f, token)) {
        if (token == "Immune System:") {
            continue;
        } else if (token == "Infection:") {
            isInfection = true;
            continue;
        } else if (token == "") {
            continue;
        }

        std::regex_search(token, matches, parser);

        for (int i = 0; i < matches.size(); ++i) {
            std::cout << i << ": " << matches[i] << std::endl;
        }

        std::pair<std::vector<std::string>, std::vector<std::string>> ws = parseWS(matches[3]);

        Army army {
            std::stoi(matches[1]),
            std::stoi(matches[2]),
            ws.second,
            ws.first,
            std::stoi(matches[9]), 
            matches[10],
            std::stoi(matches[11])
        };

        if (isInfection) {
            infection.push_back(army);
        } else {
            immune.push_back(army);
        }
    }

    for (int i = 0; i < immune.size(); ++i) {
        std::cout << i << ": " << immune[i] << std::endl;
    }

    for (int i = 0; i < infection.size(); ++i) {
        std::cout << i << ": " << infection[i] << std::endl;
    }

    int count = 0;

    while (infection.size() > 0 && immune.size() > 0) {
        std::cout << "Sorting" << std::endl;
        std::sort(infection.begin(), infection.end(), EPComparator);
        std::sort(immune.begin(), immune.end(), EPComparator);
        std::vector<int> infTargets;
        std::vector<int> immTargets;

        for (int i = 0; i < infection.size(); ++i) {
            infTargets.push_back(chooseTarget(infection[i], immune, infTargets));
        }

        for (int i = 0; i < immune.size(); ++i) {
            immTargets.push_back(chooseTarget(immune[i], infection, immTargets));
        }
        std::cout << "Sorted" << std::endl;

        // sort all by initiative
        std::vector<IIS> initID;

        for (int i = 0; i < infection.size(); ++i) {
            initID.push_back({infection[i].initiative, i, 1});
        }

        for (int i = 0; i < immune.size(); ++i) {
            initID.push_back({immune[i].initiative, i, 0});
        }

        std::sort(initID.begin(), initID.end(), INComparator);

        // perform attacks
        for (int i = 0; i < initID.size(); ++i) {
            if (initID[i].side == 0) {
                if (immTargets[initID[i].index] == -1) {
                    // doesn't attack this turn
                    continue;
                } else if (immune[initID[i].index].units <= 0) {
                    // is already dead
                    continue;
                }
                // attacker is on immune side
                attack(immune[initID[i].index], infection[immTargets[initID[i].index]]);
                std::cout << "after: " << std::endl;
                std::cout << infection[immTargets[initID[i].index]] << std::endl;
            } else {
                if (infTargets[initID[i].index] == -1) {
                    // doesn't attack this turn
                    continue;
                } else if (infection[initID[i].index].units <= 0) {
                    // is already dead
                    continue;
                }
                // attacker is on infection side
                attack(infection[initID[i].index], immune[infTargets[initID[i].index]]);
                std::cout << "after: " << std::endl;
                std::cout << immune[infTargets[initID[i].index]] << std::endl;
            }
        }

        immune.erase(std::remove_if(immune.begin(), immune.end(), [](Army& a){return a.units <= 0;}), immune.end());
        infection.erase(std::remove_if(infection.begin(), infection.end(), [](Army& a){return a.units <= 0;}), infection.end());

        std::cout << "Immune size: " << immune.size() << std::endl;
        std::cout << "Infection size: " << infection.size() << std::endl;

        count++;
    }

    int result = 0;

    for (auto &i: immune) {
        std::cout << i.units << std::endl;
        result += i.units;
    }

    for (auto &i: infection) {
        std::cout << i.units << std::endl;
        result += i.units;
    }

    std::cout << result << std::endl;

    return 0;
}