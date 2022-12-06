import heapq
import numpy as np
from typing import Set, Tuple

#################################################
# Import
#################################################
with open("2021/data/day15.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]
    data = np.array([list(row) for row in data], dtype=int)

#################################################
# Prepare
#################################################


def increase(i): return np.vectorize(lambda x: (x - 1 + i) % 9 + 1)


data = np.concatenate([increase(i)(data) for i in range(5)])
data = np.concatenate([increase(i)(data) for i in range(5)], axis=1)
n = len(data)

#################################################
# Process
#################################################


def neighbour(p: Tuple[int, int]) -> Set[Tuple[int, int]]:
    x, y = p
    return {
        (x + dx, y + dy)
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]
        if 0 <= x + dx < n and 0 <= y + dy < n
    }


def shortest_path(data: np.array) -> np.array:
    path = np.zeros(data.shape, dtype=int)
    path[0, 0] = data[0, 0]
    queue = [(data[0, 0], (0, 0))]

    while len(queue) > 0:
        # Find nearest active node
        dt, node = heapq.heappop(queue)

        # Add neighbours to queue
        for new_node in neighbour(node):
            if path[new_node] == 0:
                heapq.heappush(queue, (data[new_node] + dt, new_node))
                path[new_node] = data[new_node] + dt

    return path


path = shortest_path(data)

#################################################
# Output
#################################################
print(f"{path[-1, -1] - 1 = }")
