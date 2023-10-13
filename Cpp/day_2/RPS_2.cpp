#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int player_wins(char opponent_move, char game_result)
{
    int opponent_move_index = (int)opponent_move - (int)'A';

    switch (game_result)
    {
    case 'X': // LOSE
        return ((opponent_move_index + 2) % 3) + 1;

    case 'Y': // DRAW
        return 3 + opponent_move_index + 1;

    case 'Z': // WIN
        return 6 + ((opponent_move_index + 1) % 3) + 1;
    default:
        exit(1);
    }
}

int main()
{
    char lhs;
    char rhs;

    int points = 0;
    while (cin >> lhs && cin >> rhs)
    {
        points += player_wins(lhs, rhs);
    }

    cout << points << endl;

    return 0;
}