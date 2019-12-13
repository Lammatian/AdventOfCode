import sys
from enum import Enum
from collections import defaultdict

class Moon:
    def __init__(self, cmd):
        self.pos = list(map(lambda x: int(x.split('=')[1]), cmd.strip('<>').split(',')))
        self.vel = [0, 0, 0]

    def move(self):
        self.pos = [p + v for p, v in zip(self.pos, self.vel)]

    def adjust_vel(self, moons):
        adj = [0, 0, 0]

        for moon in moons:
            for i, (p, m_p) in enumerate(zip(self.pos, moon.pos)):
                if p < m_p:
                    adj[i] += 1
                elif p > m_p:
                    adj[i] -= 1

        self.vel = [v + a for v, a in zip(self.vel, adj)]

    def energy(self):
        return self.kin_energy() * self.pot_energy()

    def kin_energy(self):
        return sum(map(abs, self.vel))

    def pot_energy(self):
        return sum(map(abs, self.pos))

    def __str__(self):
        return 'Moon | pos=<x={}, y={}, z={} | vel=<x={}, y={}, z={}>'.format(*self.pos, *self.vel)
    
    def __repr__(self):
        return self.__str__()


def sol1(moons, steps=1000):
    for i in range(steps):
        for m in moons:
            m.adjust_vel(moons)
        
        for m in moons:
            m.move()

    return sum([m.energy() for m in moons])


def moons_to_tuple(moons, dim):
    """
    Convert moons data to a hashable tuple
    Only use data about dimension `dim` (0-indexed)
    """
    positions  = tuple([m.pos[dim] for m in moons])
    velocities = tuple([m.vel[dim] for m in moons])
    return (positions, velocities)


def steps_to_repeat(moons, dimension):
    positions = set([moons_to_tuple(moons, dimension)])

    steps = 1 # adjustment for 0 based count
    while True:
        for m in moons:
            m.adjust_vel(moons)

        for m in moons:
            m.move()
        
        cur_positions = moons_to_tuple(moons, dimension)

        if cur_positions in positions:
            return steps

        steps += 1


def gcd(a, b):
    """
    GCD of two numbers
    """
    while b != 0:
        a, b = b, a % b

    return a


def lcm(a, b):
    """
    LCM of two numbers
    """
    return (a * b) // gcd(a, b)


def mlcm(nums):
    """
    LCM of multiple numbers
    """
    current_lcm = nums[0]

    for i in range(1, len(nums)):
        current_lcm = lcm(current_lcm, nums[i])

    return current_lcm


def sol2(moons, steps=10000):
    steps = []

    for i in range(3):
        steps.append(steps_to_repeat(moons, i))

    return mlcm(steps)


def main():
    steps1 = int(sys.argv[2]) if len(sys.argv) >= 3 else 1000

    with open(sys.argv[1]) as f:
        moons = list(map(lambda x: Moon(x), f.read().splitlines()))
        
    print(sol1(moons[:], steps1))

    steps2 = int(sys.argv[3]) if len(sys.argv) >= 4 else 1000

    with open(sys.argv[1]) as f:
        moons = list(map(lambda x: Moon(x), f.read().splitlines()))

    print(sol2(moons[:], steps2))


if __name__ == '__main__':
    main()