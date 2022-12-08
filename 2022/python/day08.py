from enum import Enum
import numpy as np

#################################################
# Import
#################################################
with open("2022/data/day08.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]
    data = np.array([list(map(int, row)) for row in data])


#################################################
# Prepare
#################################################
class Direction(Enum):
    LEFT = 1
    RIGHT = 2
    UP = 3
    DOWN = 4

    def get(self, i: int, j: int, data: np.array):
        match self:
            case self.LEFT: return np.flip(data[i, :j])
            case self.RIGHT: return data[i, j + 1:]
            case self.UP: return np.flip(data[:i, j])
            case self.DOWN: return data[i + 1:, j]


def is_visible(i, j, data):
    return max(
        np.all(data[i, j] > direction.get(i, j, data))
        for direction in Direction
    )


def calc_distance(row):
    if len(row) == 0:
        return 0
    # If nothing is blocked, all are visible
    if np.all(~row):
        return len(row)
    return np.argmax(row) + 1


def calc_scenery(i, j, data):
    return np.prod([
        calc_distance(data[i, j] <= direction.get(i, j, data))
        for direction in Direction
    ])


#################################################
# Process
#################################################
output1 = sum(is_visible(i, j, data) for i, j in np.ndindex(data.shape))
output2 = max(calc_scenery(i, j, data) for i, j in np.ndindex(data.shape))


#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
