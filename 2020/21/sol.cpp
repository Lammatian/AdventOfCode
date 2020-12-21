#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <regex>
#include <algorithm>

#include "util.h"

using namespace std;

struct Food {
    set<string> ingredients;
    set<string> allergens;
};

vector<Food> prep(vector<string> lines) {
    vector<Food> foods;
    regex re("^(.*) \\(contains (.*)\\)$");
    smatch matches;

    for (auto& line: lines) {
        regex_search(line, matches, re);
        auto ingredients = util::split(matches[1], " ");
        auto allergens = util::split(matches[2], ", ");

        Food food;
        food.ingredients = set<string>(ingredients.begin(), ingredients.end());
        food.allergens = set<string>(allergens.begin(), allergens.end());

        foods.push_back(food);
    }

    return foods;
}

ll sol1(const vector<Food>& foods) {
    set<string> all_allergens;
    set<string> all_ingredients;
    map<string, set<string>> all2ing;

    for (auto& food: foods) {
        all_ingredients.insert(food.ingredients.begin(), food.ingredients.end());
        
        for (auto& allergen: food.allergens) {
            if (all2ing.find(allergen) == all2ing.end()) {
                all2ing[allergen] = food.ingredients;
            } else {
                util::intersection_ip(all2ing[allergen], food.ingredients);
            }
        }
    }

    set<string> bad_ingredients;

    for (auto& entry: all2ing) {
        bad_ingredients.insert(entry.second.begin(), entry.second.end());
    }

    ll result = 0;

    for (auto& food: foods) {
        result += food.ingredients.size() - util::intersection(bad_ingredients, food.ingredients).size();
    }

    return result;
}

string sol2(const vector<Food>& foods) {
    set<string> all_allergens;
    set<string> all_ingredients;
    map<string, set<string>> all2ing;

    for (auto& food: foods) {
        all_ingredients.insert(food.ingredients.begin(), food.ingredients.end());
        
        for (auto& allergen: food.allergens) {
            if (all2ing.find(allergen) == all2ing.end()) {
                all2ing[allergen] = food.ingredients;
            } else {
                util::intersection_ip(all2ing[allergen], food.ingredients);
            }
        }
    }

    set<string> bad_ingredients;

    for (auto& entry: all2ing) {
        bad_ingredients.insert(entry.second.begin(), entry.second.end());
    }

    ll fixed = 0;
    vector<tuple<string, string>> allergen_ingredient;
    // Assume that at any point there will be at least one allergen with
    // exactly one ingredient that causes it
    while (fixed < all2ing.size()) {
        // Find an allergen with just one possibility
        for (auto& entry: all2ing) {
            if (entry.second.size() == 1) {
                fixed++;
                auto allergen = entry.first;
                auto ingredient = *entry.second.begin();
                allergen_ingredient.push_back(make_tuple(allergen, ingredient));

                // `ingredient` was fixed for the allergen, remove it from everywhere
                for (auto& entry: all2ing) {
                    entry.second.erase(ingredient);
                }
            }
        }
    }

    sort(allergen_ingredient.begin(), allergen_ingredient.end());
    string result = "";
    for_each(allergen_ingredient.begin(), allergen_ingredient.end(),
             [&result](auto ai){
                 result += (result != "" ? "," : "") + get<1>(ai);
             });

    return result;
}

int main(int argc, char** argv) {
    string filename = argc > 1 ? argv[1] : "input.txt";
    ifstream f(filename);

    vector<Food> foods = prep(util::readlines(f));

    cout << sol1(foods) << endl;
    cout << sol2(foods) << endl;

    return 0;
}
