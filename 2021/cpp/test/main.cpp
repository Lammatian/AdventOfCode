#include "../utils.h"

int main()
{
    Print("This", "is", "a", "test");
    Print<'|'>("This", "is", "another", "test");
    int x = 1;
    double d = 3.3;
    Print("This", x, "double", d);
    vector<int> v {1, 2, 3, 4, 5};
    PrintCont(v);
    PrintCont<';'>(v);
    return 0;
}
