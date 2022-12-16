from pathlib import Path
from typing import Set, List, Tuple


#################################################
# Prepare
#################################################
Pos = Tuple[int, int]


def create_path(start: Pos, end: Pos) -> Set[Pos]:
    diff = max(abs(start[i] - end[i]) for i in range(2)) + 1
    sign0 = (start[0] < end[0]) - (start[0] > end[0])
    sign1 = (start[1] < end[1]) - (start[1] > end[1])
    return {(start[0] + sign0 * d, start[1] + sign1 * d) for d in range(diff)}


def parse(data=List[str]) -> Set[Pos]:
    mapping = set()
    for line in data:
        coords = line.split(" -> ")
        coords = [tuple(map(int, coord.split(","))) for coord in coords]
        for idx in range(len(coords) - 1):
            mapping.update(create_path(coords[idx], coords[idx + 1]))
    return mapping


def dropping_sand(mapping: Set[Pos], trace: List[Pos], max_depth: int = 0, infinite_on_end: bool = True) -> List[Pos]:
    if len(trace) == 0:
        return None

    while True:
        sand = trace[len(trace) - 1]
        if (sand[1] == max_depth + 1) and infinite_on_end:
            # Ignore sand if it falls into abyss
            return []
        elif sand[1] == max_depth + 1:
            # Stop if sand falls to bottom
            return trace
        elif (sand[0], sand[1] + 1) not in mapping:
            # One step down
            trace.append((sand[0], sand[1] + 1))
        elif (sand[0] - 1, sand[1] + 1) not in mapping:
            # Diagonal to left
            trace.append((sand[0] - 1, sand[1] + 1))
        elif (sand[0] + 1, sand[1] + 1) not in mapping:
            # Diagonal to right
            trace.append((sand[0] + 1, sand[1] + 1))
        else:
            # Come to rest
            return trace


def simulate(mapping: Set[Pos], initial: Pos = (500, 0), infinite_on_end: bool = True):
    # side effect: input parameter mapping is updated after run
    max_depth = max(pos[1] for pos in mapping)
    sands = [initial]
    while sands := dropping_sand(mapping, sands, max_depth, infinite_on_end):
        mapping.add(sands.pop())

    return


#################################################
# Import
#################################################
data = Path("2022/data/day14.txt").read_text().split("\n")
mapping = parse(data)
n_rocks = len(mapping)

#################################################
# Process
#################################################
INITIAL = (500, 0)
simulate(mapping, INITIAL, True)
output1 = len(mapping) - n_rocks

simulate(mapping, INITIAL, False)
output2 = len(mapping) - n_rocks

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
