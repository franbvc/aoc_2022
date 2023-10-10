#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
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

    auto lastThreeBegin = calories.end() - 3;
    auto lastThreeEnd = calories.end();

    cout << accumulate(lastThreeBegin, lastThreeEnd, 0, plus<int>()) << endl;

    return 0;
}