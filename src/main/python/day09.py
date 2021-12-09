import numpy as np
import pandas as pd
from collections import Counter
from typing import List

# Import
with open("data/day09.txt", "r") as f:
    data = np.array([[int(i) for i in list(row.strip())] for row in f.readlines()])

# Prepare
n, m = data.shape
rel_diff = (
    (np.diff(data, 1, 1, append=10) <= 0).astype(int) +
    (np.diff(-data, 1, 1, prepend=-10) <= 0).astype(int) +
    (np.diff(-data, 1, 0, prepend=-10) <= 0).astype(int) +
    (np.diff(data, 1, 0, append=10) <= 0).astype(int)
)
lowest_points = np.argwhere(rel_diff == 0) + 1
data_bounded = np.ones((n + 2, m + 2)) * 10
data_bounded[1:-1, 1:-1] = data

# Process
def find_route(p: List[int], data: np.array) -> int:
    cur_value = data[p[0], p[1]]
    if cur_value >= 9:
        return []

    check_points = [[p[0] + 1, p[1]], [p[0], p[1] + 1], [p[0] - 1, p[1]], [p[0], p[1] - 1]]
    acc = [p]
    for p_next in check_points:
        if data[p_next[0], p_next[1]] > cur_value:
            acc += find_route(p_next, data)

    return acc

basins = sorted([
    pd.DataFrame(find_route(point.tolist(), data_bounded)).drop_duplicates().shape[0]
    for point in lowest_points
], reverse=True)
highest_basins = basins[:3]

# Output
print(f"{np.prod(highest_basins) = }")
