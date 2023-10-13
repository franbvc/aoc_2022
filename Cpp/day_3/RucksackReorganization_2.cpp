#include <iostream>
#include <string>
#include <unordered_set>

using namespace std;

char find_common(string s1, string s2, string s3)
{
    unordered_set<char> s1_set(s1.begin(), s1.end());
    unordered_set<char> s2_set(s2.begin(), s2.end());

    for (char c : s3)
    {
        if (s1_set.count(c) > 0 && s2_set.count(c) > 0)
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
    string s1, s2, s3;
    tuple<string, string> halves;

    int count = 0;
    char currDuplicate;
    while (cin >> s1 && cin >> s2 && cin >> s3)
    {
        currDuplicate = find_common(s1, s2, s3);
        count += get_char_value(currDuplicate);
    }

    cout << count << endl;

    return 0;
}