#include <iostream>
#include <string>
#include <limits>

using namespace std;

int mod(long a, int m) {
	int r = a % m;
	return r >= 0 ? r : r + m;
}

int main()
{
  long a = 618;
  long b = 814;
  int fa = 16807;
  int fb = 48271;
  int m = numeric_limits<int>::max();
  cout << m << endl;
  int judgea = -1;
  int judgeb = -1;
  int judged = 0;

  int same = 0;

  while (judged < 5000000) {
  	while (a%4 != 0)
  		a = mod(a*fa, m);
  	while (b%8 != 0)
  		b = mod(b*fb, m);

  	judged++;

  	if (a%65536 == b%65536)
  		same++;

  	a = mod(a*fa, m);
  	b = mod(b*fb, m);
  }

  cout << same << endl;
}