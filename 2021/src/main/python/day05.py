import numpy as np
from typing import Tuple

#################################################
# Import
#################################################
def parse(text: str) -> Tuple[int, int]:
    coord = text.split(",")
    return (int(coord[0]), int(coord[1]))

with open("src/main/resources/day05.txt", "r") as f:
    data = [row.strip().split(" -> ") for row in f.readlines()]
    data = [[parse(start), parse(end)] for start, end in data]

#################################################
# Prepare
#################################################
n = max([max(row[0][0], row[1][0]) for row in data]) + 1
m = max([max(row[0][1], row[1][1]) for row in data]) + 1

#################################################
# Process
#################################################
mapping = np.zeros((n, m), dtype=int)
for start, end in data:
    dx = end[0] - start[0]
    dy = end[1] - start[1]
    steps = max(abs(dx), abs(dy))
    for s in range(steps + 1):
        mapping[start[0] + np.sign(dx) * s, start[1] + np.sign(dy) * s] += 1

n_points = len(mapping[mapping > 1])

#################################################
# Output
#################################################
print(f"{n_points = }")
