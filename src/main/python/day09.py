import numpy as np


# Import
with open("src/main/resources/day09.txt", "r") as f:
    data = np.array([[int(i) for i in list(row.strip())] for row in f.readlines()])

# Prepare
n, m = data.shape
rel_diff = np.zeros((n, m))
rel_diff[:, :-1] += (np.diff(data, 1, 1) <= 0).astype(int) ## higher than right?
rel_diff[:, 1:] += (np.diff(-data, 1, 1) <= 0).astype(int) ## higher than left?
rel_diff[:-1, :] += (np.diff(data, 1, 0) <= 0).astype(int) ## higher than down?
rel_diff[1:, :] += (np.diff(-data, 1, 0) <= 0).astype(int) ## higher then up?

lowest_points = np.argwhere(rel_diff == 0) + 1
data_bounded = np.ones((n + 2, m + 2)) * 10
data_bounded[1:-1, 1:-1] = data

# Process
def find_route(x: int, y: int, data: np.array) -> set:
    def scan_if_higher(x: int, y: int, cur_value: int, data: np.array) -> set:
        return find_route(x, y, data) if data[x, y] > cur_value else {}

    if data[x, y] >= 9:
        return {}
    else:
        acc = {(x, y)}
        acc = set.union(acc, scan_if_higher(x + 1, y, data[x, y], data)) ## go down
        acc = set.union(acc, scan_if_higher(x - 1, y, data[x, y], data)) ## go up
        acc = set.union(acc, scan_if_higher(x, y + 1, data[x, y], data)) ## go right
        acc = set.union(acc, scan_if_higher(x, y - 1, data[x, y], data)) ## got left
        return acc

basins = [len(find_route(point[0], point[1], data_bounded)) for point in lowest_points]
basins.sort(reverse=True)
highest_basins = basins[:3]

# Output
print(f"{np.prod(highest_basins) = }")
