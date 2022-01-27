#include <iostream>
#include <exception>
#include <vector>
#include <cassert>

struct InvalidInstruction : public std::exception {
    const char* what() const throw() {
        return "Unknown instruction";
    }
};

enum class Instruction {
    forward,
    down,
    up
};

Instruction fromString(std::string s) {
    if (s == "forward") {
        return Instruction::forward;
    }
    else if (s == "down") {
        return Instruction::down;
    }
    else if (s == "up") {
        return Instruction::up;
    }
    else {
        throw InvalidInstruction();
    }
}

struct Command {
    Instruction ins;
    int value;
};

class Pilot {
    std::vector<Command> commands;

  public:
    Pilot(std::istream& is) {
        std::string ins;
        int val;
        while (is >> ins) {
            is >> val;
            commands.push_back({fromString(ins), val});
        }
    }

    int determine_simple_position() {
        int horizontal = 0;
        int depth = 0;
        for (const auto& [ins, value] : commands) {
            switch (ins) {
                case Instruction::forward:
                    horizontal += value;
                    break;
                case Instruction::down:
                    depth += value;
                    break;
                case Instruction::up:
                    depth -= value;
                    break;
            }

            assert(depth >= 0);
        }

        return horizontal * depth;
    }

    int determine_aiming_position() {
        int horizontal = 0;
        int depth = 0;
        int aim = 0;
        for (const auto& [ins, value] : commands) {
            switch (ins) {
                case Instruction::forward:
                    horizontal += value;
                    depth += aim * value;
                    break;
                case Instruction::down:
                    aim += value;
                    break;
                case Instruction::up:
                    aim -= value;
                    break;
            }

            assert(depth >= 0);
        }

        return horizontal * depth;
    }
};

int main() {
    Pilot pilot(std::cin);
    std::cout << pilot.determine_simple_position() << std::endl;
    std::cout << pilot.determine_aiming_position() << std::endl;

    return 0;
}
