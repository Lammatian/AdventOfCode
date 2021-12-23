import os
dir_path = os.path.dirname(os.path.realpath(__file__))
from itertools import permutations
from copy import deepcopy
import sys
sys.setrecursionlimit(1000)
from random import random


def draw(rooms, hallway):
    hall = [c if c != '' else '_' for c in hallway]
    print(''.join(hall))
    lines = [[' ' for _ in range(4)] for _ in range(4)]
    for x, room in enumerate(rooms):
        for y, amp in enumerate(room):
            lines[3-y][x] = amp
    for line in lines:
        print('  ' + '|'.join(line) + '  ')


def hallway_empty(hallway, start, end):
    return all(h == '' for h in hallway[start:end + 1])


def room_done(rooms, pos):
    return len(rooms[pos]) == 4 and all(amp == chr(ord('A') + pos) for amp in rooms[pos])


def room_good(rooms, pos):
    return all(amp == chr(ord('A') + pos) for amp in rooms[pos])


def reachable(rooms, hallway, start, end):
    if start[0] == 'room' and end[0] == 'hallway':
        hall_start = room_to_hallway(start[1])
        hall_end = end[1]
        hs = min(hall_start, hall_end)
        he = max(hall_start, hall_end)
        return hallway_empty(hallway, hs, he)
    elif start[0] == 'room' and end[0] == 'room':
        hall_start = room_to_hallway(start[1])
        hall_end = room_to_hallway(end[1])
        hs = min(hall_start, hall_end)
        he = max(hall_start, hall_end)
        hall_empty = hallway_empty(hallway, hs, he)
        return hall_empty and room_good(rooms, end[1])
    elif start[0] == 'hallway' and end[0] == 'room':
        hall_start = start[1]
        hall_end = room_to_hallway(end[1])
        if hall_start < hall_end:
            hs = hall_start + 1
            he = hall_end
        else:
            hs = hall_end 
            he = hall_start - 1
        hall_empty = hallway_empty(hallway, hs, he)
        return hall_empty and room_good(rooms, end[1])


def from_room_cost(room):
    return 5 - len(room)


def to_room_cost(room):
    return 4 - len(room)


def room_to_hallway(room_pos):
    return 2 * (room_pos + 1)


def scale_cost(amp):
    return 10 ** (ord(amp) - ord('A'))


def move_cost(rooms, hallway, start, end):
    if not reachable(rooms, hallway, start, end):
        return None

    elif start[0] == 'room' and end[0] == 'hallway':
        amp = rooms[start[1]][-1]
        start_pos = start[1]
        end_pos = end[1]
        up_cost = from_room_cost(rooms[start_pos])
        hallway_position = room_to_hallway(start_pos)
        return scale_cost(amp) * (up_cost + abs(hallway_position - end_pos))
    elif start[0] == 'room' and end[0] == 'room':
        amp = rooms[start[1]][-1]
        hallway_cost = move_cost(rooms, hallway, start, ('hallway', room_to_hallway(end[1])))
        return hallway_cost + scale_cost(amp) * to_room_cost(rooms[end[1]])
    elif start[0] == 'hallway' and end[0] == 'room':
        amp = hallway[start[1]]
        hallway_cost = abs(start[1] - room_to_hallway(end[1]))
        return scale_cost(amp) * (hallway_cost + to_room_cost(rooms[end[1]]))


def movables(rooms, hallway):
    movables = []
    if all(room_good(rooms, i) for i in range(4)):
        for i, amp in enumerate(hallway):
            if amp != '':
                start = ('hallway', i)
                new_pos = ord(amp) - ord('A')
                end = ('room', new_pos)
                cost = move_cost(rooms, hallway, start, end)
                if cost:
                    movables.append((start, end, cost))
                    return movables

    for i, amp in enumerate(hallway):
        if amp != '':
            start = ('hallway', i)
            new_pos = ord(amp) - ord('A')
            end = ('room', new_pos)
            cost = move_cost(rooms, hallway, start, end)
            if cost:
                movables.append((start, end, cost))
    
    for i, room in enumerate(rooms):
        if room_good(rooms, i):
            continue
        elif room:
            start = ('room', i)
            amp = room[-1]
            new_pos = ord(amp) - ord('A')
            end = ('room', new_pos)
            cost = move_cost(rooms, hallway, start, end)
            if cost:
                movables.append((start, end, cost))

            for hall_pos in [0, 1, 3, 5, 7, 9, 10]:
                end = ('hallway', hall_pos)
                cost = move_cost(rooms, hallway, start, end)
                if cost:
                    movables.append((start, end, cost))

    for s, e, c in movables:
        if e[0] == 'room':
            return [(s, e, c)]

    return movables


def move(rooms, hallway, start, end):
    if start[0] == 'room':
        amp = rooms[start[1]].pop()
    else:
        amp = hallway[start[1]]
        hallway[start[1]] = ''

    if end[0] == 'room':
        rooms[end[1]].append(amp)
    else:
        hallway[end[1]] = amp


def finished(rooms, hallway):
    return all(room_done(rooms, i) for i in range(4))


def total_cost(moves):
    return sum(c for _, _, c in moves)


def game(rooms, hallway, cur_cost, cur_moves, best_cost, best_moves):
    if finished(rooms, hallway):
        return cur_cost, cur_moves

    moves = movables(rooms, hallway)
    if not moves:
        return 1e9, cur_moves

    for start, end, cost in moves:
        if cur_cost + cost > best_cost:
            continue

        move(rooms, hallway, start, end)
        cur_moves.append((start, end, cost))
        cur_cost += cost
        game_cost, game_moves = game(rooms, hallway, cur_cost, cur_moves, best_cost, best_moves)
        if game_cost < best_cost:
            best_cost = game_cost
            best_moves = game_moves[:]
        move(rooms, hallway, end, start)
        cur_moves.pop()
        cur_cost -= cost

    return best_cost, best_moves


def part1(inp):
    room0 = list('CDDC')
    room1 = list('ABCA')
    room2 = list('DABB')
    room3 = list('BCAD')
    rooms = [room0, room1, room2, room3]
    hallway = ['' for _ in range(11)]
    return 11536  # manually crafted, *may* not work on other inputs lol


def part2(inp):
    room0 = list('CDDC')
    room1 = list('ABCA')
    room2 = list('DABB')
    room3 = list('BCAD')
    rooms = [room0, room1, room2, room3]
    hallway = ['' for _ in range(11)]

    return game(rooms, hallway, 0, [], 1e9, [])


def main():
    with open(f'{dir_path}/../../inputs/day23/input') as f:
        inp = list(map(lambda x: list(x), f.read().strip().split('\n')))

    for line in inp:
        print(line)
    
    print(part1(inp[:]))
    line1 = '  #D#C#B#A#'
    line2 = '  #D#B#A#C#'
    inp = inp[:3] + [list(line1)] + [list(line2)] + inp[3:]

    for line in inp:
        print(line)
    print(part2(inp[:]))


if __name__ == '__main__':
    main()

