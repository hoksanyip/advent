from enum import Enum
import heapq
import string
from typing import Dict, Set, Tuple

#################################################
# Prepare
#################################################
ORDER = 'ES' + string.ascii_lowercase
Position = Tuple[int, int]


class Direction(Enum):
    LEFT, RIGHT, UP, DOWN = "LRUD"

    def move(self, pos: Position) -> Position:
        match self:
            case self.LEFT: return (pos[0], pos[1] - 1)
            case self.RIGHT: return (pos[0], pos[1] + 1)
            case self.UP: return (pos[0] - 1, pos[1])
            case self.DOWN: return (pos[0] + 1, pos[1])


#################################################
# Import
#################################################
with open("2022/data/day12.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]
    data = {
        (i, j): ORDER.index(cell)
        for i, row in enumerate(data)
        for j, cell in enumerate(row)
    }

#################################################
# Process
#################################################
start = next(pos for pos, cell in data.items() if cell == 1)
end = next(pos for pos, cell in data.items() if cell == 0)


def find_trail_reverse(end: Position, start: Set[Position], data: Dict[Position, int]) -> int:
    seen = set()
    queue = [(0, end)]
    while True:
        # In case of dead end, no solution possible
        if len(queue) == 0:
            # return number higher than possible, so it won't be selected as best path
            return len(data) + 1
        # Get lowest cost in queue
        cost, current = heapq.heappop(queue)
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
            heapq.heappush(queue, (cost + 1, option))
            if option in start:
                return cost + 1


output1 = find_trail_reverse(end, {start}, data)
candidates = set(pos for pos, cell in data.items() if cell in [1, 2])
output2 = find_trail_reverse(end, candidates, data)

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
