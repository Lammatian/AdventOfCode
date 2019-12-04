#include <iostream>
#include <fstream>
#include <string>
#include <vector>

typedef long long ll;

struct Instruction {
    std::string name;
    ll a;
    ll b;
    ll c;
};

int main(int argc, char* argv[]) {
    std::fstream f("input" + std::string(argv[1]) + ".txt");
    std::vector<Instruction> program;
    std::vector<ll> regs = {1, 0, 0, 0, 0, 0};
    std::string token;
    int ip = 0;

    getline(f, token);
    ip = std::stoi(token.substr(token.length() - 1));

    while (getline(f, token, ' ')) {
        Instruction i = {token, 0, 0, 0};
        getline(f, token, ' ');
        i.a = std::stol(token);
        getline(f, token, ' ');
        i.b = std::stol(token);
        getline(f, token);
        i.c = std::stol(token);
        program.push_back(i);
    }

    int pc = 0;
    int count = 0;

    while (pc >= 0 && pc < program.size() && count < 20) {
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

    // The program counts the sum of the divisors of the number in R2
    // Initialization in instructions 17-25 sets R2 to 887
    // Additional initialization is in lines 27-35 and is unlocked with R0 = 1
    // this makes R2 way bigger
    // The main loop is in lines 3-11 and checks if R3 is a divisor of R2
    // by checking whether k*R3 = R2 (k is saved in R5, k*R5 in R1)
    // If we reach k such that k > R2 then we move on to next R3 (lines 12-15)

    int target = regs[2];
    long long sum = 0;

    for (int i = 1; i <= target; ++i) {
        if (target % i == 0) {
            sum += i;
        }
    }

    std::cout << sum << std::endl;

    return 0;
}
