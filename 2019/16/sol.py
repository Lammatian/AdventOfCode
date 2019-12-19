import sys
import numpy as np
from math import ceil

def make_pattern(l, r):
    """
    Make pattern of length `l` and `r` repetitions of each
    element
    """
    batches = l // (4 * r) + 1
    result = np.resize(np.repeat([0, 1, 0, -1], r), l + 1)
    return result[1:]


def make_patterns(l):
    """
    Make all the required patterns of length `l`
    """
    result = []

    for i in range(1, l + 1):
        result.append(make_pattern(l, i))

    return np.array(result)


def sol1(data, iterations=100):
    patterns = make_patterns(len(data))
    data = np.array(data)

    for i in range(iterations):
        data = abs(patterns @ data) % 10

    return ''.join(map(str, data[:8]))


def sol2(data, iterations=100):
    offset = int(''.join(map(str, data[:7])))
    length = len(data) * 10000 - offset
    start = np.array([1] * length)

    for i in range(iterations - 1):
        start = np.cumsum(start) % 10

    result = []
    # Make proper data
    offset_data = data[offset % len(data):] + (length // len(data)) * data

    for i in range(8):
        result.append(start[:len(start)-i].dot(offset_data[i:]) % 10)

    return ''.join(map(str, result))


def main():
    with open(sys.argv[1]) as f:
        data = [int(x) for x in f.read().strip()]

    print(sol1(data))
    print(sol2(data))


if __name__ == '__main__':
    main()