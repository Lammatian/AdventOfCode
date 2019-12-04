#include <iostream>
#include <fstream>

using namespace std;

int main(){
	int sum = 0;
	string inp;
	string line;

	ifstream is("input.txt");
	if (is.is_open()) {
		while (getline(is, line))
			inp = line;
		is.close();
	}
	else
		cout << "Cannot open file" << endl;

	int jump = inp.length()/2;

	for (int i=0; i<inp.length(); i++)
		if (inp.at(i%inp.length()) == inp.at((i+jump)%inp.length()))
			sum += inp.at(i%inp.length()) - '0';

	cout << sum << endl;

	return 0;
}