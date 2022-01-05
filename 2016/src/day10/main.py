import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from collections import defaultdict, deque


def part1(inp):
    gives = {}
    chips = defaultdict(list)
    queue = deque()
    for ins in inp:
        if ins[0] == 'bot':
            bot = int(ins[1])
            low = int(ins[6])
            high = int(ins[-1])
            gives[bot] = (low, high)
        else:
            bot = int(ins[-1])
            chip = int(ins[1])
            chips[bot].append(chip)
            if len(chips[bot]) == 2:
                queue.append(bot)

    while queue:
        bot = queue.popleft()
        low, high = sorted(chips[bot])
        if low == 17 and high == 61:
            return bot

        bot_low, bot_high = gives[bot]
        chips[bot_low].append(low)
        if len(chips[bot_low]) == 2:
            queue.append(bot_low)
        chips[bot_high].append(high)
        if len(chips[bot_high]) == 2:
            queue.append(bot_high)
        del gives[bot]


def part2(inp):
    low_gives = defaultdict(int)
    high_gives = defaultdict(int)
    low_outputs = defaultdict(int)
    high_outputs = defaultdict(int)
    chips = defaultdict(list)
    queue = deque()
    for ins in inp:
        if ins[0] == 'bot':
            bot = int(ins[1])
            low = int(ins[6])
            high = int(ins[-1])
            if ins[5] == 'output':
                low_outputs[bot] = low
            else:
                low_gives[bot] = low
            if ins[-2] == 'output':
                high_outputs[bot] = high
            else:
                high_gives[bot] = high
        else:
            bot = int(ins[-1])
            chip = int(ins[1])
            chips[bot].append(chip)
            if len(chips[bot]) == 2:
                queue.append(bot)

    outputs = defaultdict(int)
    while queue:
        bot = queue.popleft()
        if 0 in outputs and 1 in outputs and 2 in outputs:
            return outputs[0] * outputs[1] * outputs[2]

        low, high = sorted(chips[bot])
        if bot in low_outputs:
            outputs[low_outputs[bot]] = low
        else:
            bot_low = low_gives[bot]
            chips[bot_low].append(low)
            if len(chips[bot_low]) == 2:
                queue.append(bot_low)
        if bot in high_outputs:
            outputs[high_outputs[bot]] = high
        else:
            bot_high = high_gives[bot]
            chips[bot_high].append(high)
            if len(chips[bot_high]) == 2:
                queue.append(bot_high)


def main():
    with open(f'{dir_path}/../../inputs/day10/input') as f:
        inp = list(map(lambda x: x.split(), f.read().strip().split('\n')))

    print(inp)
    
    print(part1(inp[:]))
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

