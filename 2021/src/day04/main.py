import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def finished(board, nums):
    for row in board:
        finished = True
        for num in row:
            if num not in nums:
                finished = False
        if finished:
            return True

    for i in range(5):
        finished = True
        for num in [row[i] for row in board]:
            if num not in nums:
                finished = False
        if finished:
            return True


def total(board, nums):
    unmarked = 0
    for row in board:
        for num in row:
            if num not in nums:
                unmarked += num

    return unmarked


def part1(nums, boards):
    for i in range(len(nums)):
        for board in boards:
            if finished(board, nums[:(i+1)]):
                return nums[i] * total(board, nums[:(i+1)])


def part2(nums, boards):
    fin_boards = []
    last_board = None
    last_idx = None

    for i in range(len(nums)):
        fin_idxs = []
        for j, board in enumerate(boards):
            if finished(board, nums[:(i+1)]):
                fin_idxs.append(j)

        for j in fin_idxs[::-1]:
            b = boards.pop(j)
            last_board = b
            last_idx = i
            fin_boards.append(b)

    return nums[last_idx] * total(last_board, nums[:(last_idx + 1)])


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

        boards.append(board)
    
    print(part1(nums, boards))
    print(part2(nums, boards))


if __name__ == '__main__':
    main()

