#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <regex>
#include <cassert>
#include <algorithm>

#include "util.h"

using namespace std;

typedef vector<ll> ticket;
typedef pair<ll, ll> range;
typedef map<string, vector<range>> rules;

enum CurrentParse {
    RULES = 0, MY_TICKET = 1, TICKETS = 2
};

ticket prep_ticket(string ticket_str) {
    vector<string> ticket_vstr = util::split(ticket_str, ",");
    ticket result(ticket_vstr.size());
    transform(ticket_vstr.begin(), ticket_vstr.end(), result.begin(),
              [](string s){return stoll(s);});

    return result;
}

tuple<rules, ticket, vector<ticket>> prep(vector<string> lines) {
    // Rules
    rules rules;
    regex re("(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)");
    smatch matches;
    CurrentParse cp = RULES;
    int idx = 0;

    while (lines[idx] != "") {
        regex_search(lines[idx], matches, re);
        range r1 = {stoll(matches[2]), stoll(matches[3])};
        range r2 = {stoll(matches[4]), stoll(matches[5])};
        rules[matches[1]] = {r1, r2};
        idx++;
    }

    // My ticket
    assert(lines[++idx] == "your ticket:");
    ticket my_ticket = prep_ticket(lines[++idx]);

    // Other tickets
    assert(lines[++idx] == "");
    assert(lines[++idx] == "nearby tickets:");
    idx++;
    vector<ticket> tickets;

    for (; idx < lines.size(); ++idx) {
        tickets.push_back(prep_ticket(lines[idx]));
    }

    return make_tuple(rules, my_ticket, tickets);
}

ll sol1(rules rules, vector<ticket> tickets) {
    vector<bool> valid(1000, false);

    for (auto& rule: rules) {
        range r1 = rule.second[0];
        range r2 = rule.second[1];

        for (int i = r1.first; i <= r1.second; ++i) {
            valid[i] = true;
        }

        for (int i = r2.first; i <= r2.second; ++i) {
            valid[i] = true;
        }
    }

    ll result = 0;

    for (auto& ticket: tickets) {
        for (auto& val: ticket) {
            if (!valid[val]) {
                result += val;
            }
        }
    }

    return result;
}

ll MINIMUM = 0;
ll MAXIMUM = 1000;

/**
 * For each value from `MINIMUM` to `MAXIMUM` get fields that it could represent
 * returns a vector with {idx -> {set of available fields for idx}}
 **/
vector<set<string>> get_valid_fields_for_values(const rules& rules) {
    vector<set<string>> valid_fields(MAXIMUM);

    for (auto& rule: rules) {
        range r1 = rule.second[0];
        range r2 = rule.second[1];

        for (int i = r1.first; i <= r1.second; ++i) {
            valid_fields[i].insert(rule.first);
        }

        for (int i = r2.first; i <= r2.second; ++i) {
            valid_fields[i].insert(rule.first);
        }
    }

    return valid_fields;
}

/**
 * Ignore the tickets that were invalid in part 1
 **/
vector<bool> get_valid_tickets(const vector<ticket>& tickets,
                               const vector<set<string>>& valid_fields) {
    vector<bool> valid_tickets(tickets.size(), true);

    for (int i = 0; i < tickets.size(); ++i) {
        for (auto& val: tickets[i]) {
            if (valid_fields[val].size() == 0) {
                valid_tickets[i] = false;
                break;
            }
        }
    }

    return valid_tickets;
}

/**
 * Given the values on the tickets, determine which fields could each entry
 * represent. Returns a vector such that
 * {i -> {all available fields for i'th entry of each ticket}}
 **/
vector<set<string>> get_valid_fields_for_tickets(const rules& rules,
                                                 const vector<ticket>& tickets,
                                                 const vector<set<string>>& valid_fields,
                                                 const vector<bool>& valid_tickets) {
    set<string> all_fields;
    for (auto& rule: rules) {
        all_fields.emplace(rule.first);
    }
    // Before we check any tickets, each entry in our ticket could be any field
    vector<set<string>> possible_fields(tickets[0].size(), all_fields);

    for (int i = 0; i < tickets.size(); ++i) {
        if (!valid_tickets[i]) {
            continue;
        }

        for (int j = 0; j < tickets[i].size(); ++j) {
            set<string> new_pf;
            set_intersection(possible_fields[j].begin(),
                             possible_fields[j].end(),
                             valid_fields[tickets[i][j]].begin(),
                             valid_fields[tickets[i][j]].end(),
                             inserter(new_pf, new_pf.begin()));
            possible_fields[j] = new_pf;
        }
    }

    return possible_fields;
}

ll sol2(const rules& rules, const ticket& my_ticket, vector<ticket> tickets) {
    // i -> all fields allowed for value i
    vector<set<string>> valid_fields = get_valid_fields_for_values(rules);
    tickets.push_back(my_ticket);
    // i -> is ticket[i] valid
    vector<bool> valid_tickets = get_valid_tickets(tickets, valid_fields);
    // i -> valid fields for the i'th ticket entry
    vector<set<string>> possible_fields =
        get_valid_fields_for_tickets(rules, tickets, valid_fields, valid_tickets);

    ll fixed = 0;
    ll result = 1;

    // Assume that at any point there will be at least one entry with exactly
    // one available field
    while (fixed < my_ticket.size()) {
        // Find an entry with just one possibility
        for (int i = 0; i < my_ticket.size(); ++i) {
            if (possible_fields[i].size() == 1) {
                fixed++;
                string field = (*possible_fields[i].begin());

                if (field.substr(0, 2) == "de") {
                    result *= my_ticket[i];
                }

                // `field` was fixed at position i, remove it from everywhere
                for (auto& pf: possible_fields) {
                    pf.erase(field);
                }
            }
        }
    }

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    rules rules;
    ticket my_ticket;
    vector<ticket> tickets;
    tie(rules, my_ticket, tickets) = prep(util::readlines(f));

    cout << sol1(rules, tickets) << endl;
    cout << sol2(rules, my_ticket, tickets) << endl;

    return 0;
}
