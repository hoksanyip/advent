from typing import Tuple

#################################################
# Import
#################################################
with open("2022/data/day04.txt", "r") as f:
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
def is_subset(set1: Tuple[int, int], set2: Tuple[int, int]) -> bool:
    return (set1[0] - set2[0]) * (set2[1] - set1[1]) >= 0


def has_overlap(set1: Tuple[int, int], set2: Tuple[int, int]) -> bool:
    return (set1[1] - set2[0]) * (set2[1] - set1[0]) >= 0


#################################################
# Output
#################################################
print(f"Answer 1: {sum([is_subset(*line) for line in data_pairs])}")
print(f"Answer 2: {sum([has_overlap(*line) for line in data_pairs])}")
