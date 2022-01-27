#include <iostream>
#include <vector>
#include <exception>
#include <utility>
#include <cassert>
#include <unordered_set>

class InvalidDigit : public std::exception {
  public:
    const char* what() const throw() {
        return "Unexpected binary digit";
    }
};

class Binary {
    std::vector<uint8_t> value;

  public:
    Binary(const std::vector<uint8_t>& bin_value) {
        value = bin_value;
    }

    Binary(std::string s) {
        for (auto& c : s) {
            switch (c) {
                case '0':
                    value.push_back(false);
                    break;
                case '1':
                    value.push_back(true);
                    break;
                default:
                    throw InvalidDigit();
            }
        }
    }

    size_t size() const {
        return value.size();
    }

    uint8_t& operator[](int i) {
        return value[i];
    }

    int to_value() {
        int result = 0;
        for (auto& b : value) {
            result *= 2;
            result += b ? 1 : 0;
        }

        return result;
    }

    using iterator = std::vector<uint8_t>::iterator;
    using const_iterator = std::vector<uint8_t>::const_iterator;
    iterator begin() { return value.begin(); }
    const_iterator begin() const { return value.begin(); }
    iterator end() { return value.end(); }
    const_iterator end() const { return value.end(); }

    friend std::ostream& operator<<(std::ostream& os, Binary& binary) {
        for (auto& b : binary) {
            // Necessary conversion as uint8_t is a typedef for unsigned char
            os << (int)b;
        }

        return os;
    }
};

class DiagnosticUnit {
    std::vector<Binary> numbers;
    int N;

    enum class RatingType {
        oxygen_generator,
        CO2_scrubber
    };
    
  public:
    DiagnosticUnit(std::istream& is) {
        for (std::string s; is >> s;) {
            numbers.push_back(Binary(s));
        }
        N = numbers.size();
    }

    std::pair<Binary, Binary> get_epsilon_and_gamma_rates() {
        assert(!numbers.empty());
        std::vector<int> ones_count(numbers[0].size(), 0);
        for (auto& bin : numbers) {
            for (int i = 0; i < bin.size(); ++i) {
                ones_count[i] += bin[i];
            }
        }

        std::vector<uint8_t> epsilon;
        std::vector<uint8_t> gamma;
        for (auto& c : ones_count) {
            epsilon.push_back(c >= N / 2 ? 1 : 0);
            gamma.push_back(c >= N / 2 ? 0 : 1);
        }

        return {Binary(epsilon), Binary(gamma)}; 
    }

    int calculate_power_consumption() {
        auto [epsilon, gamma] = get_epsilon_and_gamma_rates();
        return epsilon.to_value() * gamma.to_value();
    }

    Binary get_rating(RatingType rating) {
        std::unordered_set<int> remaining;
        for (int i = 0; i < N; ++i) {
            remaining.insert(i);
        }

        int digit_index = 0;
        while (remaining.size() > 1) {
            int ones_count = 0;
            for (const auto& num_index : remaining) {
                ones_count += numbers[num_index][digit_index];
            }

            int target;
            if (rating == RatingType::oxygen_generator) {
                target = (2 * ones_count >= remaining.size()) ? 1 : 0;
            }
            else {
                target = (2 * ones_count >= remaining.size()) ? 0 : 1;
            }

            std::vector<int> to_remove;
            for (const auto& num_index : remaining) {
                if (numbers[num_index][digit_index] != target) {
                    to_remove.push_back(num_index); 
                }
            }

            for (auto num_index : to_remove) {
                remaining.erase(num_index);
            }

            digit_index++;
        }

        assert(remaining.size() == 1);
        return numbers[*remaining.begin()];
    }

    int calculate_life_support_rating() {
        Binary oxygen_generator_rating = get_rating(RatingType::oxygen_generator);
        Binary CO2_scrubber_rating = get_rating(RatingType::CO2_scrubber);
        return oxygen_generator_rating.to_value() * CO2_scrubber_rating.to_value();
    }
};

int main() {
    DiagnosticUnit diagnostic_unit(std::cin);
    std::cout << diagnostic_unit.calculate_power_consumption() << std::endl;
    std::cout << diagnostic_unit.calculate_life_support_rating() << std::endl;

    return 0;
}
