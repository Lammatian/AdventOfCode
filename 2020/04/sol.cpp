#include <iostream>
#include <fstream>
#include <set>
#include <map>
#include <iterator>
#include <algorithm>
#include <regex>

#include "util.h"

std::vector<char> DELIMS(8, ' ');

class Passport {
    static const std::set<std::string> eye_colours;
    static const std::set<std::string> required_fields;

  public:
    std::map<std::string, std::string> fields;

    Passport(const std::string& passport_str) {
        std::vector<std::string> entries = util::tokenize(passport_str, DELIMS);

        for (auto& entry : entries) {
            std::vector<std::string> tokens = util::tokenize(entry, {':'});
            fields[tokens[0]] = tokens[1];
        }
    }

    bool has_all_required_fields() {
        for (auto& required_field : required_fields) {
            if (fields.find(required_field) == fields.end()) {
                return false;
            }
        }

        return true;
    }

    bool is_valid() {
        return has_all_required_fields()
            && validate_birth_year()
            && validate_issue_year()
            && validate_expiration_year()
            && validate_height()
            && validate_hair_colour()
            && validate_eye_colour()
            && validate_passport_id();
    }

    bool validate_birth_year() {
        try {
            int byr = std::stoi(fields["byr"]);

            return 1920 <= byr && byr <= 2002;
        } catch (...) {
            return false;
        }
    }

    bool validate_issue_year() {
        try {
            int iyr = std::stoi(fields["iyr"]);

            return 2010 <= iyr && iyr <= 2020;
        } catch (...) {
            return false;
        }
    }

    bool validate_expiration_year() {
        try {
            int eyr = std::stoi(fields["eyr"]);

            return 2020 <= eyr && eyr <= 2030;
        } catch (...) {
            return false;
        }
    }

    bool validate_height() {
        std::regex cm_regex ("^[0-9]{3}cm$");
        std::regex in_regex ("^[0-9]{2}in$");
        std::string hgt = fields["hgt"];

        if (std::regex_match(hgt, cm_regex)) {
            int cm_height = std::stoi(hgt.substr(0, 3));

            return 150 <= cm_height && cm_height <= 193;
        } else if (std::regex_match(hgt, in_regex)) {
            int in_height = std::stoi(hgt.substr(0, 2));

            return 59 <= in_height && in_height <= 76;
        } else {
            return false;
        }
    }

    bool validate_hair_colour() {
        std::regex hair_regex ("^#[0-9a-f]{6}$");

        return std::regex_match(fields["hcl"], hair_regex);
    }

    bool validate_eye_colour() {
        return eye_colours.find(fields["ecl"]) != eye_colours.end();
    }

    bool validate_passport_id() {
        std::regex pid_regex ("^[0-9]{9}$");

        return std::regex_match(fields["pid"], pid_regex);
    }

    void print() {
        for (auto& field : fields) {
            std::cout << field.first << ": " << field.second << " | ";
        }
        std::cout << std::endl;
    }
};

const std::set<std::string> Passport::eye_colours = {
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth"
};

const std::set<std::string> Passport::required_fields = {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
};

std::vector<Passport> prep(const std::vector<std::string>& lines) {
    std::vector<Passport> result;
    std::string current;

    int i = 0;
    for (auto& line : lines) {
        if (line == "") {
            result.push_back(Passport(current));
            current = "";
        } else {
            current += (current == "" ? "" : " ") + line;
        }

        i++;
    }

    if (current != "") {
        result.push_back(Passport(current));
    }

    return result;
}

ll sol1(std::vector<Passport> passports) {
    ll result = 0;

    for (auto& passport : passports) {
        if (passport.has_all_required_fields()) {
            result++;
        }
    }

    return result;
}

ll sol2(std::vector<Passport> passports) {
    ll result = 0;

    for (auto& passport : passports) {
        if (passport.is_valid()) {
            result++;
        }
    }

    return result;
}

int main() {
    std::ifstream f("input.txt");
    std::vector<Passport> passports = prep(util::readlines(f));

    std::cout << sol1(passports) << std::endl;
    std::cout << sol2(passports) << std::endl;

    return 0;
}