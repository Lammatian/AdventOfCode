#include <iostream>
#include <fstream>
#include <regex>
#include <map>
#include <algorithm>
#include <cassert>

#include "util.h"

using namespace std;

using Rules = map<int, string>;

tuple<map<int, string>, vector<string>> prep(vector<string> lines) {
    map<int, string> rules;
    vector<string> messages;

    regex re("^(\\d+): (.*)$");
    smatch matches;
    int idx = 0;

    for (; lines[idx] != ""; ++idx) {
        auto line = lines[idx];
        regex_search(line, matches, re);
        rules[stoi(matches[1])] = matches[2];
    }

    assert(lines[idx++] == "");

    for (; idx < lines.size(); ++idx) {
        messages.push_back(lines[idx]);
    }

    return make_tuple(rules, messages);
}

// list(map(int, rule_str.split(" ")))
vector<int> rule_to_keys(string rule_str) {
    vector<string> split_rule_str = util::split(rule_str, " ");
    vector<int> rules(split_rule_str.size());
    transform(split_rule_str.begin(), split_rule_str.end(), rules.begin(),
              [](string s){return stoi(s);});
    return rules;
}

bool matches_rules(string message, int pos, const Rules& rules, const vector<int>& cur_rules) {
    if (cur_rules.empty()) {
        return pos == message.size();
    }

    string first_rule = rules.at(cur_rules[0]);

    if (first_rule.find("|") != first_rule.npos) {
        vector<string> split_rule = util::split(first_rule, " | "); 
        vector<int> new_rules1 = rule_to_keys(split_rule[0]);
        new_rules1.insert(new_rules1.end(), cur_rules.begin() + 1, cur_rules.end());
        vector<int> new_rules2 = rule_to_keys(split_rule[1]);
        new_rules2.insert(new_rules2.end(), cur_rules.begin() + 1, cur_rules.end());
        return matches_rules(message, pos, rules, new_rules1)
            || matches_rules(message, pos, rules, new_rules2);
    } else if (first_rule.find("\"") != first_rule.npos) {
        vector<int> new_rules = cur_rules;
        // Despite this, vector seems to be faster than list
        new_rules.erase(new_rules.begin());
        return message[pos] == first_rule[1]
            && matches_rules(message, pos + 1, rules, new_rules);
    } else {
        vector<int> new_rules = rule_to_keys(first_rule);
        new_rules.insert(new_rules.end(), cur_rules.begin() + 1, cur_rules.end());
        return matches_rules(message, pos, rules, new_rules);
    }
}

ll sol1(Rules rules, vector<string> messages) {
    ll result = 0;

    for (auto& msg: messages) {
        if (matches_rules(msg, 0, rules, rule_to_keys(rules[0]))) {
            result++;
        }
    }

    return result;
}

ll sol2(Rules rules, vector<string> messages) {
    ll result = 0;
    rules[8] = "42 | 42 8";
    rules[11] = "42 31 | 42 11 31";

    for (auto& msg: messages) {
        if (matches_rules(msg, 0, rules, rule_to_keys(rules[0]))) {
            result++;
        }
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    Rules rules;
    vector<string> messages;
    tie(rules, messages) = prep(util::readlines(f));

    cout << sol1(rules, messages) << endl;
    cout << sol2(rules, messages) << endl;

    return 0;
}