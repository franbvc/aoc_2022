#include <iostream>
#include <string>
#include <tuple>

using namespace std;

tuple<int, int> parse_range(string s)
{
    int dash_pos = s.find('-');
    return make_tuple(
        stoi(s.substr(0, dash_pos)),
        stoi(s.substr(dash_pos + 1)));
}

tuple<tuple<int, int>, tuple<int, int>> parse_line(string s)
{
    int comma_pos = s.find(',');

    string first = s.substr(0, comma_pos);
    string second = s.substr(comma_pos + 1);

    return make_tuple(
        parse_range(first),
        parse_range(second));
}

bool is_range_contained(tuple<int, int> lhs, tuple<int, int> rhs)
{
    int lhs_l = get<0>(lhs);
    int lhs_r = get<1>(lhs);
    int rhs_l = get<0>(rhs);
    int rhs_r = get<1>(rhs);

    if (lhs_l <= rhs_l && lhs_r >= rhs_l)
        return true;

    if (rhs_l <= lhs_l && rhs_r >= lhs_l)
        return true;

    return false;
}

int main()
{
    string s;
    tuple<int, int> lhs, rhs;
    tuple<tuple<int, int>, tuple<int, int>> line;

    int count = 0;
    while (cin >> s)
    {
        line = parse_line(s);
        lhs = get<0>(line);
        rhs = get<1>(line);

        count += is_range_contained(lhs, rhs);
    }

    cout << count << endl;

    return 0;
}