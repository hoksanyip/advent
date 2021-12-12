import numpy as np

#################################################
# Import
#################################################
with open("2021/src/main/resources/day09.txt", "r") as f:
    data = np.array([list(row.strip()) for row in f.readlines()], dtype=int)

#################################################
# Prepare
#################################################
n, m = data.shape
rel_diff = np.zeros((n, m))
rel_diff[:, :-1] += (np.diff(-data, 1, 1) > 0).astype(int)  ## higher than right?
rel_diff[:, 1:] += (np.diff(data, 1, 1) > 0).astype(int)    ## higher than left?
rel_diff[:-1, :] += (np.diff(-data, 1, 0) > 0).astype(int)  ## higher than down?
rel_diff[1:, :] += (np.diff(data, 1, 0) > 0).astype(int)    ## higher then up?

lowest_points = np.argwhere(rel_diff == 0) + 1
data_bounded = np.ones((n + 2, m + 2)) * 10
data_bounded[1:-1, 1:-1] = data

#################################################
# Process
#################################################
def extend_basin(x: int, y: int, prev_value: int, data: np.array) -> set:
    cur_value = data[x, y]
    if cur_value >= 9 or cur_value <= prev_value:
        return {}
    else:
        acc = {(x, y)}
        acc = set.union(acc, extend_basin(x + 1, y, cur_value, data)) ## go down
        acc = set.union(acc, extend_basin(x - 1, y, cur_value, data)) ## go up
        acc = set.union(acc, extend_basin(x, y + 1, cur_value, data)) ## go right
        acc = set.union(acc, extend_basin(x, y - 1, cur_value, data)) ## got left
        return acc

basins = [len(extend_basin(point[0], point[1], -1, data_bounded)) for point in lowest_points]
basins.sort(reverse=True)
highest_basins = basins[:3]

#################################################
# Output
#################################################
print(f"{np.prod(highest_basins) = }")
