from random import random

class InputGenerator():
    def __init__(self, ascii_alphabet_path):
        with open(ascii_alphabet_path) as f:
            ascii_alphabet = f.readlines()

        self.letter_mappings =\
            InputGenerator._generate_letter_mappings(ascii_alphabet)

    def generate_input(self, text, num_folds):
        """
        Given a string and (optionally) a number of folds, generate an AoC-like
        input for day 13 that will result in this input being generated when
        ran on a working implementation
        """
        coords = self._map_text_to_coords(text)
        folds = InputGenerator._generate_folds(coords, num_folds)
        str_input = ''

        for x, y in InputGenerator._unfold(folds[::-1], coords):
            str_input += f'{x},{y}\n'

        str_input += '\n'

        for fd, fv in folds:
            str_input += f'fold along {fd}={fv}\n'

        return str_input

    @staticmethod
    def _generate_letter_mappings(ascii_alphabet):
        """
        Given a multi-line string (parsed as list) with 4x6 ASCII-letters,
        produce mappings from their string counterparts to their coordinates in
        ASCII. e.g. given letter L as

        #...
        #...
        #...
        #...
        #...
        ####

        this will produce an entry

        'L': {(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 6),
        (2, 6), (3, 6)}

        in the output
        """
        mapping = {}
        for i in range(ord('Z') - ord('A') + 1):
            letter = chr(ord('A') + i)
            start_x = 5 * i
            letter_ascii = [line[start_x:start_x+5] for line in ascii_alphabet]
            coords = set()
            for y, line in enumerate(letter_ascii):
                for x, c in enumerate(line):
                    if c == '#':
                        coords.add((x, y))
                
            mapping[letter] = coords

        return mapping

    def _map_text_to_coords(self, text):
        """
        Given a text, map it to coordinates using the generated mappings
        
        Assumes a width of 4 characters (and 1 character of spacing)
        """
        coords = set()
        for i, c in enumerate(text):
            letter_coords = self.letter_mappings[c]

            for x, y in letter_coords:
                coords.add((x + 5 * i, y))

        return coords

    @staticmethod
    def _generate_folds(coords, num_folds):
        """
        Given coordinates and the number of folds, generate a randomised list
        of folds of the form (direction, value) along which the input will be
        then folded.
        """
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

    @staticmethod
    def _unfold(folds, coords):
        """
        Given a (ordered) list of folds and the coordinates, randomly unfold
        the coordinates so that for each fold and each coordinate, it will be
        present at least on one side of the fold
        """
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

