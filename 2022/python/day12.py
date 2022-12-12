from enum import Enum
import string
from typing import Dict, Set, Tuple

#################################################
# Prepare
#################################################
ORDER = string.ascii_lowercase
Position = Tuple[int, int]


class Direction(Enum):
    LEFT, RIGHT, UP, DOWN = (0, -1), (0, 1), (-1, 0), (1, 0)

    def move(self, pos: Position) -> Position:
        return (pos[0] + self.value[0], pos[1] + self.value[1])


#################################################
# Import
#################################################
with open("2022/data/day12.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]
    data = {
        (i, j): cell
        for i, row in enumerate(data)
        for j, cell in enumerate(row)
    }
    # Find start and end points
    start = next(pos for pos, cell in data.items() if cell == 'S')
    end = next(pos for pos, cell in data.items() if cell == 'E')
    # Update with elevation
    data[start] = 'a'
    data[end] = 'z'
    # Transform text to height number
    data = {pos: ORDER.index(cell) for pos, cell in data.items()}


#################################################
# Process
#################################################
def find_trail_reverse(end: Position, start: Set[Position], data: Dict[Position, int]) -> int:
    seen = set()
    queue = [(0, end)]
    while True:
        # Get lowest cost in queue
        cost, current = queue.pop(0)
        if current in seen:
            continue
        seen.add(current)

        # Get next moves
        options = {
            option
            # for all directions
            for option in {direction.move(current) for direction in Direction}
            if option not in seen  # which are new
            and option in data.keys()  # and within grid
            # and current is max max 1 higher
            and data[current] - data[option] <= 1
        }

        # Add to queue
        for option in options:
            queue.append((cost + 1, option))
            if option in start:
                return cost + 1


output1 = find_trail_reverse(end, {start}, data)
candidates = set(pos for pos, cell in data.items() if cell == 0)
output2 = find_trail_reverse(end, candidates, data)

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
