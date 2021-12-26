import curses
import traceback
from time import sleep

class Visualiser():
    def __init__(self, window, res_x, res_y, animation_time, animation_frames):
        # Can set own resolution
        curses.endwin()
        self.window = window
        self.RES_X = res_x - 1
        self.RES_Y = res_y - 1
        self.ANIMATION_TIME = animation_time
        self.FRAMES = animation_frames
        self.SQUARE = chr(9608)

    @staticmethod
    def _squeeze_coords_to_window(coords, res_x, res_y, max_x, max_y):
        window_coords = set()
        for x, y in coords:
            wx = res_x * x // max_x
            wy = res_y * y // max_y
            window_coords.add((wx, wy))

        return window_coords

    def animate_fold(self, coords, fold):
        vmax_x = max([x for x, y in coords])
        vmax_y = max([y for x, y in coords])
        res_x = min(self.RES_X, vmax_x)
        res_y = min(self.RES_Y, vmax_y)

        if fold[0] == 'x':
            max_x = 2 * fold[1]
            max_y = vmax_y
        else:
            max_x = vmax_x
            max_y = 2 * fold[1]

        window_coords = Visualiser._squeeze_coords_to_window(
                coords, res_x, res_y, max_x, max_y)

        fd, fv = fold
        if fd == 'x':
            fx = res_x * fold[1] // max_x
            fy = None
        else:
            fx = None
            fy = res_y * fold[1] // max_y

        fold_coords = set()
        for x, y in window_coords:
            if (fx and x > fx) or (fy and y > fy):
                fold_coords.add((x, y))

        for frame in range(self.FRAMES):
            frame_fold_coords = set()
            for x, y in fold_coords:
                move_x = 2 * (x - fx) if fx else 0
                move_y = 2 * (y - fy) if fy else 0
                cx = x - move_x * frame // self.FRAMES
                cy = y - move_y * frame // self.FRAMES
                frame_fold_coords.add((cx, cy))

            display_coords = (window_coords - fold_coords).union(frame_fold_coords)
            display_fold = (fd, fx if fx else fy)
            to_display = self.generate_display(
                    res_x, res_y, display_coords, display_fold)
            self.display(to_display)
            self.window.refresh()
            sleep(self.ANIMATION_TIME / self.FRAMES)

    def animate_stretch(self, coords, prev_fold):
        vmax_x = max(x for x, y in coords)
        vmax_y = max(y for x, y in coords)

        # Don't stretch if there is no reason to
        if 2 * vmax_x + 1 < self.RES_X and 2 * vmax_y + 1 < self.RES_Y:
            return
        elif prev_fold[0] == 'y' and 2 * vmax_y + 1 < self.RES_Y:
            return
        elif prev_fold[0] == 'x' and 2 * vmax_x + 1 < self.RES_X:
            return

        if prev_fold[0] == 'x':
            res_x = min(2 * vmax_x + 1, self.RES_X)
            max_x = 2 * vmax_x + 1
            res_y = min(vmax_y, self.RES_Y)
            max_y = vmax_y
            fx = prev_fold[1]
            fy = None
        else:
            res_x = min(vmax_x, self.RES_X)
            max_x = vmax_x
            res_y = min(2 * vmax_y + 1, self.RES_Y)
            max_y = 2 * vmax_y + 1
            fx = None
            fy = prev_fold[1]

        window_coords = Visualiser._squeeze_coords_to_window(
                coords, res_x, res_y, max_x, max_y)

        for frame in range(self.FRAMES):
            frame_stretch_coords = set()
            for x, y in window_coords:
                move_x = (x + 1) * frame // self.FRAMES if fx else 0
                move_y = (y + 1) * frame // self.FRAMES if fy else 0
                cx = x + move_x
                cy = y + move_y
                frame_stretch_coords.add((cx, cy))

            to_display = self.generate_display(
                    res_x, res_y, frame_stretch_coords)
            self.display(to_display)
            self.window.refresh()
            sleep(self.ANIMATION_TIME / self.FRAMES)
            self.window.clear()

        sleep(self.ANIMATION_TIME)

    def display_fold_result(self, coords, fold):
        vmax_x = max([x for x, y in coords])
        vmax_y = max([y for x, y in coords])
        if fold[0] == 'x':
            res_x = min(2 * vmax_x + 1, self.RES_X)
            max_x = 2 * vmax_x + 1
            res_y = min(vmax_y, self.RES_Y)
            max_y = vmax_y
        else:
            res_x = min(vmax_x, self.RES_X)
            max_x = vmax_x
            res_y = min(2 * vmax_y + 1, self.RES_Y)
            max_y = 2 * vmax_y + 1

        window_coords = Visualiser._squeeze_coords_to_window(
                coords, res_x, res_y, max_x, max_y)

        to_display = self.generate_display(res_x, res_y, window_coords)
        self.display(to_display)
        self.window.refresh()
        sleep(self.ANIMATION_TIME)

    def display_final_result(self, coords):
        max_x = max([x for x, y in coords])
        max_y = max([y for x, y in coords])
        res_x = min(max_x, self.RES_X)
        res_y = min(max_y, self.RES_Y)

        window_coords = Visualiser._squeeze_coords_to_window(
                coords, res_x, res_y, max_x, max_y)

        self.window.clear()
        to_display = self.generate_display(res_x, res_y, window_coords)
        self.display(to_display)
        self.window.refresh()
        sleep(20)

    def generate_display(self, res_x, res_y, coords, fold=None):
        display = [[' ' for _ in range(res_x + 1)] for _ in range(res_y + 1)]
        colours = [['W' for _ in range(res_x + 1)] for _ in range(res_y + 1)]

        for dx, dy in coords:
            display[dy][dx] = self.SQUARE

        if fold:
            fd, fv = fold
            if fd == 'x':
                for y in range(res_y):
                    display[y][fv] = '|'
                    colours[y][fv] = 'R'
            else:
                for x in range(res_x):
                    display[fv][x] = '-'
                    colours[fv][x] = 'R'

        return display, colours

    def display(self, colour_display):
        display, colours = colour_display
        for y, (display_line, line_colours) in enumerate(zip(display, colours)):
            for x, (v, c) in enumerate(zip(display_line, line_colours)):
                try:
                    if c == 'R':
                        self.window.addstr(y, x, v, curses.COLOR_RED)
                    else:
                        self.window.addstr(y, x, v, curses.COLOR_WHITE)
                except curses.error:
                    curses.endwin()
                    print(x, y)
                    exit(1)

            try:
                if y != len(display) - 1:
                    self.window.addstr(y, len(display_line), '\n')
            except curses.error:
                curses.endwin()
                print(x, y, self.RES_X, self.RES_Y)
                exit(1)

