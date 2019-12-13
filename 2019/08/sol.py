import sys
from collections import Counter

def get_image(data, n, w, h):
    """
    Get n-th (counting from 0) image of width `w` and height
    `h` from the dataset
    """
    start_idx = n * w * h
    end_idx = (n + 1) * w * h

    return data[start_idx:end_idx]


def sol1(data):
    w = 25
    h = 6
    least_zeroes = w * h
    best_product = 0
    best_tuple = (0, 0)

    for i in range(len(data) // (w * h)):
        c = Counter(get_image(data, i, w, h))

        if c[0] < least_zeroes:
            best_product = c[1] * c[2]
            best_tuple = (c[1], c[2])
            least_zeroes = c[0]

    return best_product, best_tuple


def pixel_overlay(top, bottom):
    return top if top < 2 else bottom


def overlay(data_img1, data_img2):
    return list(map(lambda x: pixel_overlay(*x), zip(data_img1, data_img2)))
    

def row_to_str(row):
    return ''.join(list(map(lambda x: '*' if x == 1 else ' ', row)))


def to_image(data_img, w, h):
    for i in range(0, w * h, w):
        print(data_img[i: i + w]) 


def to_str_image(data_img, w, h):
    for i in range(0, w * h, w):
        print(row_to_str(data_img[i: i + w]))


def sol2(data):
    w = 25
    h = 6
    img = [2] * (w * h)

    for i in range(len(data) // (w * h)):
        img = overlay(img, get_image(data, i, w, h))    

    return to_image(img, w, h), to_str_image(img, w, h)


def main():
    with open(sys.argv[1]) as f:
        data = list(map(int, f.read()[:-1]))

    print(data)
    print(sol1(data))
    print(sol2(data))

if __name__ == '__main__':
    main()