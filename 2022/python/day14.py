from pathlib import Path
from typing import Set, List, Tuple


#################################################
# Prepare
#################################################
Pos = Tuple[int, int]


def create_path(start: Pos, end: Pos) -> Set[Pos]:
    if start[0] == end[0]:
        sign = -1 if start[1] > end[1] else 1
        length = abs(start[1] - end[1])
        return {(start[0], start[1] + d * sign) for d in range(length + 1)}
    else:
        sign = -1 if start[0] > end[0] else 1
        length = abs(start[0] - end[0])
        return {(start[0] + d * sign, start[1]) for d in range(length + 1)}


def parse(data=List[str]) -> Set[Pos]:
    mapping = set()
    for line in data:
        coords = line.split(" -> ")
        coords = [tuple(map(int, coord.split(","))) for coord in coords]
        for idx in range(len(coords) - 1):
            mapping.update(create_path(coords[idx], coords[idx + 1]))
    return mapping


def dropping_sand(mapping: Set[Pos], initial: List[Pos], max_depth: int = 0, infinite_on_end: bool = True) -> List[Pos]:
    sands = initial
    n = len(initial)
    if len(initial) == 0:
        return None

    while True:
        sand = sands[len(sands) - 1]
        if (sand[1] == max_depth + 1) and infinite_on_end:
            return []
        elif sand[1] == max_depth + 1:
            return sands
        elif (sand[0], sand[1] + 1) not in mapping:
            sands.append((sand[0], sand[1] + 1))
        elif (sand[0] - 1, sand[1] + 1) not in mapping:
            sands.append((sand[0] - 1, sand[1] + 1))
        elif (sand[0] + 1, sand[1] + 1) not in mapping:
            sands.append((sand[0] + 1, sand[1] + 1))
        else:
            return sands


def simulate(mapping: Set[Pos], initial: Pos = (500, 0), infinite_on_end: bool = True):
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
