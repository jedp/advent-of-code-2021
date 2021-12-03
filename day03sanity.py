#!/usr/bin/env python3

def winnow(values, bit, shift):
    ones = set([v for v in values if v & 1<<shift])

    count = 0
    if len(ones) >= len(values) / 2:
        if bit == 1:
            print(f"{shift} return {[bin(x) for x in ones]}")
            return ones
        return set(values) - ones

    if bit == 1:
        nones = set(values) - ones
        print(f"{shift} return {[bin(x) for x in nones]}")
        return nones
    return ones


def calc_life_support(filename):
    ss = open(filename).readlines()
    print(ss)
    l = len(ss[0]) - 1 # subtract newline
    ns = [int(s, 2) for s in ss]

    oxy = ns
    for i in reversed(range(l)):
        oxy = winnow(oxy, 1, i)
        if len(oxy) < 2:
            break
    oxy_result = list(oxy)[0]

    co2 = ns
    for i in reversed(range(l)):
        co2 = winnow(co2, 0, i)
        if len(co2) < 2:
            break
    co2_result = list(co2)[0]

    return oxy_result * co2_result


if __name__ == '__main__':
    example = calc_life_support("day03.example.txt")
    print(f"Example life support: {example}")
    real = calc_life_support("day03.input.txt")
    print(f"Real life support: {real}")

