#include <iostream>
#include <string>
#include <tuple>

using namespace std;

tuple<string, string> splitInMiddle(string s)
{
    int middle = s.size() / 2;
    return make_tuple(s.substr(0, middle), s.substr(middle, s.size()));
}

char find_duplicate(string s1, string s2)
{
    for (char c : s1)
    {
        if (s2.find(c) != string::npos)
            return c;
    }

    exit(1);
}

int get_char_value(char c)
{
    if (isupper(c))
        return int(c) - int('A') + 27;

    if (islower(c))
        return int(c) - int('a') + 1;

    exit(1);
}

int main()
{
    string s;
    tuple<string, string> halves;

    int count = 0;
    char currDuplicate;
    while (cin >> s)
    {
        halves = splitInMiddle(s);
        currDuplicate = find_duplicate(get<0>(halves), get<1>(halves));
        count += get_char_value(currDuplicate);
    }

    cout << count << endl;

    return 0;
}