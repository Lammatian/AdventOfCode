#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct Instruction {
    std::string name;
    int a;
    int b;
    int c;
};

int main(int argc, char* argv[]) {
    std::fstream f("input" + std::string(argv[1]) + ".txt");
    std::vector<Instruction> program;
    std::vector<int> regs(6, 0);
    std::string token;
    int ip = 0;

    getline(f, token);
    ip = std::stoi(token.substr(token.length() - 1));

    while (getline(f, token, ' ')) {
        Instruction i = {token, 0, 0, 0};
        getline(f, token, ' ');
        i.a = std::stoi(token);
        getline(f, token, ' ');
        i.b = std::stoi(token);
        getline(f, token);
        i.c = std::stoi(token);
        program.push_back(i);
    }

    int pc = 0;
    int count = 0;

    while (pc >= 0 && pc < program.size()) {
        regs[ip] = pc;
        Instruction current = program[pc];
        std::cout << "PC: " << pc;
        std::cout << "\t ["; 
        for (int i = 0; i < 6; ++i) {
            std::cout << regs[i] << (i != 5 ? ", " : "");
        }
        std::cout << "]\t";
        std::cout << " " << current.name << " " << current.a << " " << current.b << " " << current.c;

        switch (current.name[0]) {
            case 'a': // add
                if (current.name[3] == 'i') { // addi
                    regs[current.c] = regs[current.a] + current.b;
                } else { // addr
                    regs[current.c] = regs[current.a] + regs[current.b];
                }
                break;
            case 'm':
                if (current.name[3] == 'i') { // muli
                    regs[current.c] = regs[current.a] * current.b;
                } else { // mulr 
                    regs[current.c] = regs[current.a] * regs[current.b];
                }
                break;
            case 's':
                if (current.name[3] == 'i') { // seti
                    regs[current.c] = current.a;
                } else { // setr 
                    regs[current.c] = regs[current.a];
                }
                break;
            case 'g':
                if (current.name.substr(2) == "ri") { // gtri
                    regs[current.c] = (regs[current.a] > current.b ? 1 : 0); 
                } else if (current.name.substr(2) == "ir") { // gtir
                    regs[current.c] = (current.a > regs[current.b] ? 1 : 0); 
                } else { // gtrr
                    regs[current.c] = (regs[current.a] > regs[current.b] ? 1 : 0); 
                }
                break;
            case 'e':
                if (current.name.substr(2) == "ri") { // eqri
                    regs[current.c] = (regs[current.a] == current.b ? 1 : 0); 
                } else if (current.name.substr(2) == "ir") { // eqir
                    regs[current.c] = (current.a == regs[current.b] ? 1 : 0); 
                } else { // eqrr
                    regs[current.c] = (regs[current.a] == regs[current.b] ? 1 : 0); 
                }
                break;
        }

        std::cout << "\t [";
        for (int i = 0; i < 6; ++i) {
            std::cout << regs[i] << (i != 5 ? ", " : "");
        }
        std::cout << "]" << std::endl;

        pc = regs[ip];
        pc++;
        count++;
    }

    std::cout << regs[0] << std::endl;

    return 0;
}