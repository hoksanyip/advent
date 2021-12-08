from functools import reduce
import numpy as np
from typing import List


# Import
with open("data/day08.txt", "r") as f:
    data = [row.split(" | ") for row in f.readlines()]

# Prepare
class Digit:
    order = []

    def __init__(self, parser: str):
        digits = [set(d) for d in parser.split(" ")]
        digits.sort(key=len)
        # Derive from 1 - 4 - 7 - 8
        a = digits[1] - digits[0]
        cf = digits[0]
        bd = digits[2] - digits[0]
        # Derive from 5
        with_bd = [d for d in digits if bd.issubset(d)]
        g = with_bd[1] - digits[0] - digits[1] - digits[2]
        # Derive from 9 - 6
        with_out_e = [d for d in with_bd if cf.issubset(d) and with_bd[1].issubset(d)]
        e = with_out_e[1] - with_out_e[0]
        c = with_out_e[0] - with_bd[1]
        f = cf - c
        # Derive from 2 - 3
        b = bd - [d for d in digits if len(d) == 5 and c.issubset(d)][0]
        d = bd - b
        # Define all digits by order
        self.order = [
            set.union(a, b, c, e, f, g),    # 0
            set.union(c, f),                # 1
            set.union(a, c, d, e, g),       # 2
            set.union(a, c, d, f, g),       # 3
            set.union(b, c, d, f),          # 4
            set.union(a, b, d, f, g),       # 5
            set.union(a, b, d, e, f, g),    # 6
            set.union(a, c, f),             # 7
            set.union(a, b, c, d, e, f, g), # 8
            set.union(a, b, c, d, f, g)     # 9
        ]

    def parse(self, value: str) -> int:
        digits = [self.order.index(set(d)) for d in value.strip().split(" ")]
        return reduce(lambda x, y: x * 10 + y, digits)

# Process
numbers = [Digit(parser).parse(value) for parser, value in data]

# Output
print(f"{sum(numbers) = }")
