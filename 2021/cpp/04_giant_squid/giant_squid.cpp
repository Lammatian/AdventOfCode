#include <algorithm>
#include <iostream>
#include <sstream>
#include <valarray>
#include <vector>

class BingoBoard
{
public:
    BingoBoard(std::valarray<int>&& board) : mBoard(board)
    {
        mSumUnmarked = mBoard.sum();
    }

    bool Bingo()
    {
        return IsRowDone() or IsColumnDone();
    }

    int GetSumUnmarked()
    {
        return mSumUnmarked;
    }

    void TryRemove(int number)
    {
        for (auto& entry: mBoard)
        {
            if (entry == number)
            {
                entry = 0;
                mSumUnmarked -= number;
            }
        }
    }

private:
    bool IsRowDone()
    {
        for (int i = 0; i < 5; ++i)
        {
            std::gslice gslice(5 * i, {5}, {1});
            if (std::valarray<int>(mBoard[gslice]).sum() == 0)
                return true;
        }

        return false;
    }

    bool IsColumnDone()
    {
        for (int i = 0; i < 5; ++i)
        {
            std::gslice gslice(i, {5}, {5});
            if (std::valarray<int>(mBoard[gslice]).sum() == 0)
                return true;
        }

        return false;
    }

    std::valarray<int> mBoard;
    int mSumUnmarked;
};

std::vector<int> ParseNumbers(const std::string& numberStr)
{
    std::stringstream ss(numberStr);
    std::vector<int> result;
    std::string token;
    while (getline(ss, token, ','))
    {
        result.push_back(std::stoi(token));
    }

    return result;
}

int main()
{
    std::string token;
    getline(std::cin, token);
    std::vector<int> numbers = ParseNumbers(token);

    std::vector<BingoBoard> bingoBoards;
    std::string board;
    while (getline(std::cin, token))
    {
        std::vector<int> board;
        for (int i = 0; i < 25; ++i)
        {
            int x;
            std::cin >> x;
            board.push_back(x);
        }
        std::valarray<int> boardArray(board.data(), board.size());
        bingoBoards.push_back(BingoBoard(std::move(boardArray)));
    }
    std::vector<BingoBoard> bingoBoards2 = bingoBoards;

    // Part 1
    bool done{false};
    for (auto num: numbers)
    {
        for (auto& bingoBoard: bingoBoards)
        {
            bingoBoard.TryRemove(num);
            if (bingoBoard.Bingo())
            {
                std::cout << bingoBoard.GetSumUnmarked() * num << "\n";
                done = true;
                break;
            }
        }

        if (done) break;
    }

    // Part 2
    done = false;
    int bingoCount = 0;
    for (auto num: numbers)
    {
        for (auto& bingoBoard: bingoBoards2)
        {
            if (bingoBoard.Bingo()) continue;
            bingoBoard.TryRemove(num);
            if (bingoBoard.Bingo())
                bingoCount++;
            if (bingoCount == bingoBoards2.size())
            {
                std::cout << num * bingoBoard.GetSumUnmarked() << "\n";
                done = true;
                break;
            }
        }

        if (done) break;
    }

    return 0;
}
