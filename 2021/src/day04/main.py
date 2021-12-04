import os
dir_path = os.path.dirname(os.path.realpath(__file__))

import numpy as np


def part1(nums, boards):
    for num in nums:
        for board in boards:
            board[board == num] = 0
        
            if np.count_nonzero(board.sum(axis=0)) < 5:
                return num * board.sum()
            if np.count_nonzero(board.sum(axis=1)) < 5:
                return num * board.sum()


def part2(nums, boards):
    last_board, last_num = None, None

    for num in nums:
        for board in boards:
            if np.count_nonzero(board.sum(axis=0)) < 5:
                continue
            if np.count_nonzero(board.sum(axis=1)) < 5:
                continue

            board[board == num] = 0

            if np.count_nonzero(board.sum(axis=0)) < 5:
                last_board = board
            if np.count_nonzero(board.sum(axis=1)) < 5:
                last_num = num

    return last_num * last_board.sum()


def main():
    with open(f'{dir_path}/../../inputs/day04/input') as f:
        inp = f.read().strip().split('\n')

    nums = list(map(int, inp[0].split(',')))
    inp = inp[2:]
    boards = []

    for i in range(0, len(inp), 6):
        board = []
        for j in range(5):
            board.append(list(map(int, inp[i + j].split())))

        boards.append(np.array(board))
    
    print(part1(nums, boards))
    print(part2(nums, boards))


if __name__ == '__main__':
    main()

