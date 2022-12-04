from typing import Tuple
import numpy as np

#################################################
# Import
#################################################
with open("2022/src/main/resources/day04.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]

#################################################
# Prepare
#################################################
Range = Tuple[int, int]


def get_sets(line: str) -> Tuple[Range, Range]:
    a, b = line.split(",")
    def to_set(x): return tuple(map(int, x.split("-")))
    return to_set(a), to_set(b)


data_pairs = [get_sets(line) for line in data]


#################################################
# Process
#################################################
def is_subset(a: Range, b: Range) -> bool:
    return np.sign(a[0] - b[0]) * np.sign(b[1] - a[1]) >= 0


def has_overlap(a: Range, b: Range) -> bool:
    return np.sign(a[1] - b[0]) * np.sign(b[1] - a[0]) >= 0


#################################################
# Output
#################################################
print(f"Answer 1: {sum([is_subset(*line) for line in data_pairs])}")

print(f"Answer 2: {sum([has_overlap(*line) for line in data_pairs])}")
