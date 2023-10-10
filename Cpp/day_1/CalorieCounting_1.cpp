#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main()
{
    vector<int> calories;

    std::string line;
    int currElf = 0;

    while (std::getline(std::cin, line))
    {
        if (line.empty())
        {
            calories.push_back(currElf);
            currElf = 0;
            continue;
        }

        currElf += stoi(line);
    }

    sort(calories.begin(), calories.end());
    cout << calories.back() << endl;

    return 0;
}