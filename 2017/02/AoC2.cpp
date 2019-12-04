#include <string>
#include <sstream>
#include <vector>
#include <iterator>
#include <iostream>
#include <fstream>
#include <algorithm>

template<typename Out>
void split(const std::string &s, char delim, Out result) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

int main() {
	int total = 0;
	std::string inp = "";
	std::string line;

	std::ifstream is("input.txt");
	if (is.is_open()) {
		while (getline(is, line))
			inp += line + "\n";
		is.close();
	}

	std::vector<std::string> x = split(inp, '\n');

	for (int i=0; i<x.size(); i++) {
		std::vector<std::string> nums = split(x[i], '\t');
		std::vector<int> intnums;

		for (int i=0; i<nums.size(); i++) {
			intnums.push_back(std::stoi(nums[i]));
		}

		for (int i=0; i<intnums.size(); i++) {
			for (int j=i+1; j<intnums.size(); j++) {
				if (intnums[i] % intnums[j] == 0 || intnums[j] % intnums[i] == 0) {
					total += (intnums[i] > intnums[j]) ? (intnums[i] / intnums[j]) : (intnums[j] / intnums[i]);
					goto next;
				}
			}
		}

		next:;
	}

	std::cout << total << std::endl;

	return 0;
}