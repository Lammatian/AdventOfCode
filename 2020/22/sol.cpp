#include <iostream>
#include <fstream>
#include <cassert>
#include <list>
#include <set>

#include "util.h"

using namespace std;

using Deck = list<int>;

tuple<Deck, Deck> prep(const vector<string>& lines) {
    Deck p1, p2;

    int idx = 0;
    assert(lines[idx++] == "Player 1:");

    while (lines[idx] != "") {
        p1.push_back(stoi(lines[idx++]));
    }

    assert(lines[idx++] == "");
    assert(lines[idx++] == "Player 2:");

    while (idx < lines.size()) {
        p2.push_back(stoi(lines[idx++]));
    }

    return make_tuple(p1, p2);
}

ll deck_to_score(const Deck& deck) {
    ll score = 0;
    int mult = deck.size();

    for (auto it = deck.begin(); it != deck.end(); ++it) {
        score += mult * (*it);
        mult--;
    }

    return score;
}

ll sol1(Deck p1, Deck p2) {
    while (!p1.empty() && !p2.empty()) {
        int f1 = p1.front();
        int f2 = p2.front();
        p1.pop_front();
        p2.pop_front();

        if (f1 > f2) {
            p1.push_back(f1);
            p1.push_back(f2);
        } else {
            p2.push_back(f2);
            p2.push_back(f1);
        }
    }

    return p1.empty() ? deck_to_score(p2) : deck_to_score(p1);
}

tuple<int, ll> play_game(Deck p1, Deck p2) {
    set<Deck> p1_decks;

    while (p1_decks.find(p1) == p1_decks.end() && !p1.empty() && !p2.empty()) {
        p1_decks.emplace(p1);

        int f1 = p1.front();
        int f2 = p2.front();

        p1.pop_front();
        p2.pop_front();
        int winner;

        if (f1 > p1.size() || f2 > p2.size()) {
            winner = f1 > f2 ? 1 : 2;
        } else {
            // Recurse!
            ll score;

            auto end1 = next(p1.begin(), f1);
            auto end2 = next(p2.begin(), f2);
            Deck rp1(p1.begin(), end1);
            Deck rp2(p2.begin(), end2);

            tie(winner, score) = play_game(rp1, rp2);
        }

        if (winner == 1) {
            p1.push_back(f1);
            p1.push_back(f2);
        } else {
            p2.push_back(f2);
            p2.push_back(f1);
        }
    }

    return p1.empty() ? make_tuple(2, deck_to_score(p2)) : make_tuple(1, deck_to_score(p1));
}

ll sol2(Deck p1, Deck p2) {
    int winner;
    ll score;
    tie(winner, score) = play_game(p1, p2);

    return score;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    Deck p1, p2;
    tie(p1, p2) = prep(util::readlines(f));

    cout << sol1(p1, p2) << endl;
    cout << sol2(p1, p2) << endl;

    return 0;
}

