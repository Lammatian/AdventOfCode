#include <iostream>

using namespace std;

int main() {
	int step = 354;
	int current = 0;
	int after = 0;

	for (int i=1; i<50000001; i++) {
		current = (current + step)%i + 1;
		if (current == 1)
			after = i;
	}

	cout << after << endl;
}