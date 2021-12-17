import os
import sys
dir_path = os.path.dirname(os.path.realpath(__file__))
from time import sleep
import curses
from curses import wrapper
from random import random


RES_X = 200
RES_Y = 50
FRAMES = 60
ANIMATION_LENGTH = 0.3


def generate_letter_mappings(letter_mappings_txt):
    mapping = {}

    for i in range(ord('Z') - ord('A') + 1):
        letter = chr(ord('A') + i)
        start_x = 5 * i
        letter_ascii = [line[start_x:start_x+5] for line in letter_mappings_txt]
        coords = set()

        for y, line in enumerate(letter_ascii):
            for x, c in enumerate(line):
                if c == '#':
                    coords.add((x, y))
            
        mapping[letter] = coords

    return mapping


def generate_folds(coords, num_folds):
    folds = []
    max_x = max(x for x, y in coords)
    max_y = max(y for x, y in coords)

    for _ in range(num_folds):
        fd = 'x' if random() > 0.7 else 'y'
        if fd == 'x':
            fv = max_x + 1
            max_x = 2 * fv
        else:
            fv = max_y + 1
            max_y = 2 * fv
        
        folds = [(fd, fv)] + folds

    return folds


def map_string_to_coords(letter_mapping, string):
    coords = set()
    for i, c in enumerate(string):
        letter_coords = letter_mapping[c]

        for x, y in letter_coords:
            coords.add((x + 5 * i, y))

    return coords


def generate_input(user_input, num_folds):
    with open('ascii_alphabet.txt') as f:
        letter_mappings_txt = f.readlines()

    LETTER_MAPPING = generate_letter_mappings(letter_mappings_txt)
    coords = map_string_to_coords(LETTER_MAPPING, user_input)
    folds = generate_folds(coords, num_folds)
    str_input = ''

    for x, y in generate(folds[::-1], coords):
        str_input += f'{x},{y}\n'
        #print(f'{x},{y}')

    str_input += '\n'
    #print()

    for fd, fv in folds:
        str_input += f'fold along {fd}={fv}\n'
        #print(f'fold along {fd}={fv}')

    return str_input


def generate(folds, coords):
    # Folds are in form ('x|y', value)
    for fd, fv in folds:
        old = set()
        new = set()
        for x, y in coords:
            if fd == 'x':
                flipped = (2 * fv - x, y)
            else:
                flipped = (x, 2 * fv - y)

            if flipped[0] < 0 or flipped[1] < 0:
                print(fd, fv, x, y)

            if random() < 0.5:
                old.add((x, y))

                if random() < 0.20:
                    new.add(flipped)
            else:
                new.add(flipped)

                if random() < 0.20:
                    old.add((x, y))
            
        coords = list(old) + list(new)

    return coords


def part1(ins, folds):
    visible = set(ins)
    fold = folds[0]

    to_rem = set()
    to_add = set()
    if fold[0] == 'x':
        for x, y in visible:
            if x > fold[1]:
                to_rem.add((x, y))
                to_add.add((2*fold[1] - x, y))
    if fold[0] == 'y':
        for x, y in visible:
            if y > fold[1]:
                to_rem.add((x, y))
                to_add.add((x, 2*fold[1] - y))

    for p in to_rem:
        visible.remove(p)

    for p in to_add:
        visible.add(p)

    return len(visible)


def part2(ins, folds, window):
    visible = set(ins)

    for fold in folds:
        visualise(window, RES_X, RES_Y, visible, fold, True, True)

        to_rem = set()
        to_add = set()
        if fold[0] == 'x':
            for x, y in visible:
                if x > fold[1]:
                    to_rem.add((x, y))
                    to_add.add((2*fold[1] - x, y))
        if fold[0] == 'y':
            for x, y in visible:
                if y > fold[1]:
                    to_rem.add((x, y))
                    to_add.add((x, 2*fold[1] - y))

        for p in to_rem:
            visible.remove(p)

        for p in to_add:
            visible.add(p)

        visualise(window, RES_X, RES_Y, visible, fold, True, False)

    max_x = max([x for x, y in visible])
    max_y = max([y for x, y in visible])
    board = [['.' for _ in range(max_x + 1)] for _ in range(max_y + 1)]

    for x, y in visible:
        board[y][x] = '#'

    final_visual(window, visible)

    return len(visible)


def generate_display(res_x, res_y, displayed, fold=None):
    SQUARE = chr(9608)
    RED = '\033[91m'
    WHITE = '\033[0m'
    display = [[' ' for _ in range(res_x + 1)] for _ in range(res_y + 1)]
    colours = [[(256, 256, 256) for _ in range(res_x + 1)] for _ in range(res_y + 1)]

    for dx, dy in displayed:
        display[dy][dx] = SQUARE

    if fold:
        fd, fv = fold
        if fd == 'x':
            for y in range(res_y):
                display[y][fv] = '|'
                colours[y][fv] = (256, 0, 0)
        else:
            for x in range(res_x):
                display[fv][x] = '-'
                colours[fv][x] = (256, 0, 0)

    return display, colours


def display(window, colour_display):
    display, colours = colour_display
    for y, (display_line, line_colours) in enumerate(zip(display, colours)):
        for x, (v, c) in enumerate(zip(display_line, line_colours)):
                if c == (256, 0, 0):
                    window.addstr(y, x, v, curses.COLOR_RED)
                else:
                    window.addstr(y, x, v, curses.COLOR_WHITE)
        window.addstr(y, len(display_line) + 1, '\n')


def visualise(window, res_x, res_y, visible, fold, with_fold=False, animate=True):
    vmax_x = max([x for x, y in visible])
    vmax_y = max([y for x, y in visible])
    displayed = set()

    if animate:
        res_x = min(res_x, vmax_x)
        res_y = min(res_y, vmax_y)

        if fold[0] == 'x':
            max_x = 2 * fold[1]
            max_y = vmax_y
        else:
            max_x = vmax_x
            max_y = 2 * fold[1]

        for x, y in visible:
            dx = res_x * x // max_x
            dy = res_y * y // max_y
            displayed.add((dx, dy))

        fd, fv = fold
        if fd == 'x':
            fx = res_x * fold[1] // max_x
            fy = None
        else:
            fx = None
            fy = res_y * fold[1] // max_y

        to_fold = set((x, y) for x, y in displayed if (fx and x > fx) or (fy and y > fy))

        for frame in range(FRAMES):
            frame_fold = set()
            for x, y in to_fold:
                move_x = 2 * (x - fx) if fx else 0
                move_y = 2 * (y - fy) if fy else 0
                cx = x - move_x * frame // FRAMES
                cy = y - move_y * frame // FRAMES
                frame_fold.add((cx, cy))

            to_display = (displayed - to_fold).union(frame_fold)
            display(window, generate_display(res_x, res_y, to_display, (fd, fx if fx else fy)))
            sleep(ANIMATION_LENGTH / FRAMES)
            window.refresh()
    else:
        if fold[0] == 'x':
            res_x = min(2 * vmax_x + 1, res_x)
            max_x = 2 * vmax_x + 1
            res_y = min(vmax_y, res_y)
            max_y = vmax_y
        else:
            res_x = min(vmax_x, res_x)
            max_x = vmax_x
            res_y = min(2 * vmax_y + 1, res_y)
            max_y = 2 * vmax_y + 1

        for x, y in visible:
            dx = res_x * x // max_x
            dy = res_y * y // max_y
            displayed.add((dx, dy))

        display(window, generate_display(res_x, res_y, displayed))
        window.refresh()
        sleep(ANIMATION_LENGTH)

    if not with_fold:
        visualise(res_x, res_y, visible, fold, with_fold=True)


def final_visual(window, visible):
    max_x = max([x for x, y in visible])
    max_y = max([y for x, y in visible])
    res_x = max_x
    res_y = max_y

    displayed = set()

    for x, y in visible:
        dx = res_x * x // max_x
        dy = res_y * y // max_y
        displayed.add((dx, dy))

    window.clear()
    display(window, generate_display(res_x, res_y, displayed))
    window.refresh()
    sleep(20)


def main(window):
    global RES_X, RES_Y
    user_input = sys.argv[1]
    if len(sys.argv) > 2:
        num_folds = int(sys.argv[2])
    else:
        num_folds = 12

    inp = generate_input(user_input, num_folds)
    inp = list(map(lambda x: x, inp.split('\n')))
    #with open(f'{dir_path}/../../inputs/day13/paparapa') as f:
        #inp = list(map(lambda x: x, f.read().strip().split('\n')))

    ins = [tuple(map(int, x.split(','))) for x in inp if x and x[0].isdigit()]
    folds = [(x.split('=')[0][-1], int(x.split('=')[1])) for x in inp if x and x[0] == 'f']
    print(ins, folds)
    
    curses.start_color()
    curses.use_default_colors()
    curses.curs_set(0)
    window.clear()
    RES_Y, RES_X = window.getmaxyx()
    RES_Y -= 5
    RES_X -= 3
    print(part1(ins[:], folds[:]))
    print(part2(ins[:], folds[:], window))


if __name__ == '__main__':
    wrapper(main)

