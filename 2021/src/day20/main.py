import os
dir_path = os.path.dirname(os.path.realpath(__file__))


def part1(enh, img):
    for step in range(2):
        filler = '.' if step % 2 == 0 or enh[0] == '.' else '#'
        empty_line = filler * len(img[0])
        img = [empty_line] * 2 + img + [empty_line] * 2

        for i in range(len(img)):
            img[i] = 2 * filler + img[i] + 2 * filler

        new_img = [['.' for _ in range(len(img[0]) - 2)] for _ in range(len(img) - 2)]
        for py in range(1, len(img) - 1):
            for px in range(1, len(img[0]) - 1):
                code = img[py-1][px-1:px+2] + img[py][px-1:px+2] + img[py+1][px-1:px+2]
                cval = 0
                for c in code:
                    cval *= 2
                    cval += 1 if c == '#' else 0
                new_img[py-1][px-1] = enh[cval]

        img = new_img
        for i in range(len(img)):
            img[i] = ''.join(img[i])

    return sum(line.count('#') for line in img)


def part2(enh, img):
    for step in range(50):
        filler = '.' if step % 2 == 0 or enh[0] == '.' else '#'
        empty_line = filler * len(img[0])
        img = [empty_line] * 2 + img + [empty_line] * 2

        for i in range(len(img)):
            img[i] = 2 * filler + img[i] + 2 * filler

        new_img = [['.' for _ in range(len(img[0]) - 2)] for _ in range(len(img) - 2)]
        for py in range(1, len(img) - 1):
            for px in range(1, len(img[0]) - 1):
                code = img[py-1][px-1:px+2] + img[py][px-1:px+2] + img[py+1][px-1:px+2]
                cval = 0
                for c in code:
                    cval *= 2
                    cval += 1 if c == '#' else 0
                new_img[py-1][px-1] = enh[cval]

        img = new_img
        for i in range(len(img)):
            img[i] = ''.join(img[i])

    return sum(line.count('#') for line in img)


def main():
    with open(f'{dir_path}/../../inputs/day20/input') as f:
        inp = list(map(lambda x: x, f.read().strip().split('\n\n')))
        enh, img =  inp[0], inp[1].split('\n')

    print(enh, img)
    
    print(part1(enh[:], img[:]))
    print(part2(enh[:], img[:]))


if __name__ == '__main__':
    main()

