import os
import sys
dir_path = os.path.dirname(os.path.realpath(__file__))
from time import sleep
import curses
from curses import wrapper
from random import random
from input_generator import InputGenerator
from visualiser import Visualiser


FRAMES = 60
ANIMATION_LENGTH = 0.3


def foldify(ins, folds, visualiser):
    visible = set(ins)

    for fold in folds:
        visualiser.animate_fold(visible, fold)

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

        visualiser.display_fold_result(visible, fold)
        visualiser.animate_stretch(visible, fold)

    visualiser.display_final_result(visible)


def parse_input(inp):
    inp = list(map(lambda x: x, inp.split('\n')))
    coords = [tuple(map(int, x.split(','))) for x in inp if x and x[0].isdigit()]
    folds = [(x.split('=')[0][-1], int(x.split('=')[1])) for x in inp if x and x[0] == 'f']
    return coords, folds


def main(window):
    user_input = sys.argv[1].upper()
    if len(sys.argv) > 2:
        num_folds = int(sys.argv[2])
    else:
        num_folds = 12

    ig = InputGenerator('ascii_alphabet.txt')
    inp = ig.generate_input(user_input, num_folds)
    coords, folds = parse_input(inp[:])
    
    curses.start_color()
    curses.use_default_colors()
    curses.curs_set(0)
    window.clear()
    res_y, res_x = window.getmaxyx()
    # We don't want to risk going (literally) overboard
    res_y -= 1
    res_x -= 1
    visualiser = Visualiser(window, res_x, res_y, ANIMATION_LENGTH, FRAMES)
    foldify(coords[:], folds[:], visualiser)


if __name__ == '__main__':
    wrapper(main)

