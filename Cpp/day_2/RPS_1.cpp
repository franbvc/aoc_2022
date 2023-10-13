#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int player_wins(char opponent_move, char player_move)
{
    int player_move_index = (int)player_move - (int)'X';
    int opponent_move_index = (int)opponent_move - (int)'A';

    if (player_move_index == opponent_move_index)
        return 3 + player_move_index + 1;

    if ((player_move_index + 2) % 3 == opponent_move_index)
        return 6 + player_move_index + 1;

    return player_move_index + 1;
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