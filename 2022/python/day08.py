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
    LEFT, RIGHT, UP, DOWN = range(4)

    def get(self, i: int, j: int, data: np.array):
        match self:
            case self.LEFT: return np.flip(data[i, :j])
            case self.RIGHT: return data[i, j + 1:]
            case self.UP: return np.flip(data[:i, j])
            case self.DOWN: return data[i + 1:, j]


def is_visible(i, j, data):
    """
    If in ANY direction, ALL trees are lower than current tree,
    then it is visible.

    :param i: row index
    :param j: column index
    :param data: numpy 2d array of tree map
    :return: boolean whether tree is visible from any direction
    """
    return any([
        (data[i, j] > direction.get(i, j, data)).all()
        for direction in Direction
    ])


def calc_distance(row):
    """
    Distance as defined as number of visible trees until the vision is 'blocked'

    :param row: 1d array of boolean indicating whether tree is blocking the view
    :return: distance to first blocked tree (or number of trees if it is not blocked)
    """
    # On edge, so no trees can be seen there
    if len(row) == 0:
        return 0
    # If nothing is blocked, all are visible
    if np.all(~row):
        return len(row)
    return np.argmax(row) + 1


def calc_scenery(i, j, data):
    """
    Take the product of the "distance" for all directions

    :param i: row index
    :param j: column index
    :param data: numpy 2d array of tree map
    :return: scenery value as the product of the distances
    """
    return np.prod([
        calc_distance(data[i, j] <= direction.get(i, j, data))
        for direction in Direction
    ])


#################################################
# Output
#################################################
output1 = sum(is_visible(i, j, data) for i, j in np.ndindex(data.shape))
print(f"Answer 1: {output1}")

output2 = max(calc_scenery(i, j, data) for i, j in np.ndindex(data.shape))
print(f"Answer 2: {output2}")
